package edu.knuca.resmat

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import edu.knuca.resmat.auth.{AuthService, DefaultAuthService, UUIDTokenGenerator}
import edu.knuca.resmat.db.{DBConfig, DatabaseService, FlywayService}
import edu.knuca.resmat.http.HttpRoutes
import edu.knuca.resmat.user.{DefaultUsersService, UsersService}
import edu.knuca.resmat.utils.Config
import akka.http.scaladsl.server.directives.DebuggingDirectives
import edu.knuca.resmat.data.InitialDataGenerator
import edu.knuca.resmat.exam.taskflow.TaskFlowExamService
import edu.knuca.resmat.exam.testset.TestSetExamService
import edu.knuca.resmat.exam.{ExamService, ProblemService, UserExamService}

import scala.concurrent.ExecutionContext

object Main extends App with Config {
  implicit val actorSystem = ActorSystem()
  implicit val executor: ExecutionContext = actorSystem.dispatcher
  implicit val log: LoggingAdapter = Logging(actorSystem, getClass)
  implicit val materializer: ActorMaterializer = ActorMaterializer()

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
  val testSetExamService: TestSetExamService = new TestSetExamService(databaseService)
  val problemService: ProblemService = new ProblemService(databaseService)
  val taskFlowExamService: TaskFlowExamService = new TaskFlowExamService(databaseService)(problemService)
  val examService: ExamService = new ExamService(databaseService)
  val userExamService: UserExamService = new UserExamService(databaseService)(examService, usersService, testSetExamService, taskFlowExamService)

  val dataGenerator = new InitialDataGenerator(
    databaseService, usersService, authService, examService, problemService, userExamService, testSetExamService, taskFlowExamService
  )
  if(MySql.generateDataOnStartup) {
    dataGenerator.generate()
  }

  val httpRoutes = new HttpRoutes(usersService, authService, userExamService, examService, testSetExamService, problemService)(dataGenerator)
  val routeToBind = if(requestResultLoggingEnabled) {
    DebuggingDirectives.logRequestResult("Resmat REST API", Logging.DebugLevel)(httpRoutes.routes)
  } else {
    httpRoutes.routes
  }

  Http().bindAndHandle(routeToBind, httpHost, httpPort)
}
