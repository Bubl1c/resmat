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
import edu.knuca.resmat.articles.ArticleService
import edu.knuca.resmat.data.InitialDataGenerator
import edu.knuca.resmat.exam.taskflow.TaskFlowConfAndExamService
import edu.knuca.resmat.exam.testset.TestSetExamService
import edu.knuca.resmat.exam.{ExamConfService, ProblemConfService, UserExamService}
import edu.knuca.resmat.tests.TestConfService

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

  if(!Resmat.env.isProd && MySql.migrateOnStartup) {
    val flywayService = new FlywayService(MySql.flywayJdbcUrl, MySql.user, MySql.password, MySql.db, Flyway.baselineVersion)
    if(!Resmat.env.isProd && MySql.dropSchemaOnStartup) {
      flywayService.dropDatabase()
    }
    flywayService.migrateDatabaseSchema()
  }

  val mySqlDbConfig = DBConfig(MySql.jdbcUrl, MySql.user, MySql.password, MySql.driver, MySql.conns)
  val databaseService = new DatabaseService {
    override lazy val dbConfig = mySqlDbConfig
  }

  val tokenGenerator = new UUIDTokenGenerator

  lazy val usersService: UsersService = new DefaultUsersService(databaseService)
  lazy val authService: AuthService = new DefaultAuthService(databaseService)(tokenGenerator, usersService)
  lazy val testConfService: TestConfService = new TestConfService(databaseService, s3Manager)
  lazy val testSetExamService: TestSetExamService = new TestSetExamService(databaseService, testConfService)
  lazy val problemConfService: ProblemConfService = new ProblemConfService(databaseService)
  lazy val taskFlowConfAndExamService: TaskFlowConfAndExamService = new TaskFlowConfAndExamService(databaseService)(problemConfService)
  lazy val examConfService: ExamConfService = new ExamConfService(databaseService, testConfService, taskFlowConfAndExamService)
  lazy val userExamService: UserExamService = new UserExamService(databaseService)(examConfService, usersService, testConfService, testSetExamService, taskFlowConfAndExamService)
  lazy val articleService: ArticleService = new ArticleService(databaseService, s3Manager)

  val dataGenerator = new InitialDataGenerator(
    databaseService, usersService, authService, examConfService, problemConfService, userExamService, testSetExamService, taskFlowConfAndExamService, testConfService, articleService
  )
  if(MySql.generateDataOnStartup) {
    dataGenerator.generate()
  }

  val httpRoutes = new HttpRoutes(
    usersService,
    authService,
    userExamService,
    examConfService,
    testConfService,
    testSetExamService,
    taskFlowConfAndExamService,
    problemConfService,
    articleService,
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
