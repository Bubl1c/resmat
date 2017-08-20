package edu.knuca.resmat

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import edu.knuca.resmat.auth.{AuthService, DefaultAuthService, UUIDTokenGenerator}
import edu.knuca.resmat.db.{DBConfig, DatabaseService, FlywayService}
import edu.knuca.resmat.http.HttpRoutes
import edu.knuca.resmat.user.{DefaultUsersService, UsersService}
import edu.knuca.resmat.utils.{Config, S3Manager}
import akka.http.scaladsl.server.directives.DebuggingDirectives
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.data.InitialDataGenerator
import edu.knuca.resmat.exam.taskflow.TaskFlowExamService
import edu.knuca.resmat.exam.testset.TestSetExamService
import edu.knuca.resmat.exam.{ExamService, ProblemService, UserExamService}
import edu.knuca.resmat.tests.TestConfsService

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Api extends App with Config with LazyLogging {
  implicit val actorSystem = ActorSystem()
  implicit val executor: ExecutionContext = actorSystem.dispatcher
  implicit val log: LoggingAdapter = Logging(actorSystem, getClass)
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val cfg = ConfigFactory.load("aws").getConfig("s3")
  val accessKey = cfg.getString("accessKey")
  val secretKey = cfg.getString("secretKey")
  val bucket = cfg.getString("bucket")
  val s3Manager = new S3Manager(accessKey, secretKey, bucket, "https://s3.eu-central-1.amazonaws.com")

  if(MySql.migrateOnStartup) {
    val flywayService = new FlywayService(MySql.flywayJdbcUrl, MySql.user, MySql.password, MySql.db)
    flywayService.dropDatabase()
    flywayService.migrateDatabaseSchema
  }

  val mySqlDbConfig = DBConfig(MySql.jdbcUrl, MySql.user, MySql.password, MySql.driver, MySql.conns)
  val databaseService = new DatabaseService {
    override lazy val dbConfig = mySqlDbConfig
  }

  val tokenGenerator = new UUIDTokenGenerator

  val usersService: UsersService = new DefaultUsersService(databaseService)
  val authService: AuthService = new DefaultAuthService(databaseService)(tokenGenerator, usersService)
  val testConfsService: TestConfsService = new TestConfsService(databaseService)
  val testSetExamService: TestSetExamService = new TestSetExamService(databaseService, testConfsService)
  val problemService: ProblemService = new ProblemService(databaseService)
  val taskFlowExamService: TaskFlowExamService = new TaskFlowExamService(databaseService)(problemService)
  val examService: ExamService = new ExamService(databaseService)
  val userExamService: UserExamService = new UserExamService(databaseService)(examService, usersService, testConfsService, testSetExamService, taskFlowExamService)

  val dataGenerator = new InitialDataGenerator(
    databaseService, usersService, authService, examService, problemService, userExamService, testSetExamService, taskFlowExamService, testConfsService
  )
  if(MySql.generateDataOnStartup) {
    dataGenerator.generate()
  }

  val httpRoutes = new HttpRoutes(
    usersService,
    authService,
    userExamService,
    examService,
    testConfsService,
    testSetExamService,
    problemService,
    s3Manager
  )(dataGenerator)
  val routeToBind = if(requestResultLoggingEnabled) {
    DebuggingDirectives.logRequestResult("Resmat REST API", Logging.DebugLevel)(httpRoutes.routes)
  } else {
    httpRoutes.routes
  }

  Http().bindAndHandle(routeToBind, httpHost, httpPort).onComplete {
    case Success(_) => logger.info(s"API is listening on $httpHost:$httpPort")
    case Failure(t) => logger.error("Failed to start API: ", t)
  }
}
