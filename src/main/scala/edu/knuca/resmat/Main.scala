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

import scala.concurrent.ExecutionContext

object Main extends App with Config {
  implicit val actorSystem = ActorSystem()
  implicit val executor: ExecutionContext = actorSystem.dispatcher
  implicit val log: LoggingAdapter = Logging(actorSystem, getClass)
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val flywayService = new FlywayService(MySql.flywayJdbcUrl, MySql.user, MySql.password, MySql.db)
  flywayService.dropDatabase()
  flywayService.migrateDatabaseSchema

  val mySqlDbConfig = DBConfig(MySql.jdbcUrl, MySql.user, MySql.password, MySql.driver, MySql.conns)
  val databaseService = new DatabaseService {
    override lazy val dbConfig = mySqlDbConfig
  }

  val tokenGenerator = new UUIDTokenGenerator

  val usersService: UsersService = new DefaultUsersService(databaseService)
  val authService: AuthService = new DefaultAuthService(databaseService)(tokenGenerator, usersService)

  val dataGenerator = new InitialDataGenerator(databaseService, usersService, authService)
  dataGenerator.generate()

  val httpRoutes = new HttpRoutes(usersService, authService, dataGenerator)
  val httpRouteLogged = DebuggingDirectives.logRequestResult("Resmat REST API", Logging.WarningLevel)(httpRoutes.routes)

  Http().bindAndHandle(httpRouteLogged, httpHost, httpPort)
}
