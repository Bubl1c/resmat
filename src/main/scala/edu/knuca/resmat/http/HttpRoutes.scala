package edu.knuca.resmat.http

import akka.http.scaladsl.server.Directives._
import edu.knuca.resmat.InitialDataGenerator
import edu.knuca.resmat.auth.{AuthRoute, AuthService}
import edu.knuca.resmat.students.StudentsRoute
import edu.knuca.resmat.user.{AuthenticatedUser, UsersRoute, UsersService}

import scala.concurrent.ExecutionContext

class HttpRoutes(usersService: UsersService, val authService: AuthService, val dataGenerator: InitialDataGenerator)
                (implicit executionContext: ExecutionContext)
  extends CorsSupport
    with ApiExceptionHandlers
    with ApiRejectionHandler
    with SecurityDirectives {

  val usersRouter = new UsersRoute(authService, usersService)
  val authRouter = new AuthRoute(authService, usersService, dataGenerator)
  val studentsRouter = new StudentsRoute(usersService)

  val routes =
    pathPrefix("v1") {
      (handleExceptions(generalHandler) & handleRejections(generalRejectionHandler)) {
        corsHandler {
          authRouter.route ~
          authenticate { implicit user: AuthenticatedUser =>
            usersRouter.route ~
            studentsRouter.route
          }
        }
      }
    }

}
