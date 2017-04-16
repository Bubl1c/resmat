package edu.knuca.resmat.http

import akka.http.scaladsl.server.Directives._
import edu.knuca.resmat.InitialDataGenerator
import edu.knuca.resmat.auth.{AuthRoute, AuthService}
import edu.knuca.resmat.exam._
import edu.knuca.resmat.students.StudentsRoute
import edu.knuca.resmat.user.{AuthenticatedUser, UsersRoute, UsersService}
import ch.megard.akka.http.cors.CorsDirectives._
import edu.knuca.resmat.exam.taskflow.TaskFlowExamRoute
import edu.knuca.resmat.exam.testset.{TestSetExamRoute, TestSetExamService}

import scala.concurrent.ExecutionContext

class HttpRoutes(usersService: UsersService, val authService: AuthService, val examService: UserExamService, val testSetExamService: TestSetExamService)
                (val dataGenerator: InitialDataGenerator)
                (implicit executionContext: ExecutionContext)
    extends ApiExceptionHandlers
    with ApiRejectionHandler
    with SecurityDirectives {

  val usersRouter = new UsersRoute(authService, usersService)
  val authRouter = new AuthRoute(authService, usersService, dataGenerator)
  val studentsRouter = new StudentsRoute(usersService)
  val testSetExamRouter = new TestSetExamRoute(examService)
  val taskFlowExamRouter = new TaskFlowExamRoute(examService)
  val examRouter = new ExamRoute(examService, testSetExamRouter, taskFlowExamRouter)

  val routes =
    pathPrefix("v1") {
      handleRejections(corsRejectionHandler) {
        cors() {
          (handleExceptions(generalHandler) & handleRejections(generalRejectionHandler)) {
            authRouter.route ~
              authenticate { implicit user: AuthenticatedUser =>
                usersRouter.route ~
                  studentsRouter.route ~
                  examRouter.route
              }
          }
        }
      }
    }

}
