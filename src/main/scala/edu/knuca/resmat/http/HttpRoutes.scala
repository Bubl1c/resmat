package edu.knuca.resmat.http

import akka.http.scaladsl.model.{HttpMethod, HttpMethods}
import akka.http.scaladsl.server.Directives.{complete, _}
import edu.knuca.resmat.auth.{AuthRoute, AuthService, TestConfsRoute}
import edu.knuca.resmat.exam._
import edu.knuca.resmat.students.StudentsRoute
import edu.knuca.resmat.user.{AuthenticatedUser, TmpFileUploadRoute, UsersRoute, UsersService}
import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings
import edu.knuca.resmat.data.InitialDataGenerator
import edu.knuca.resmat.exam.taskflow.TaskFlowExamRoute
import edu.knuca.resmat.exam.testset.{TestSetExamRoute, TestSetExamService}
import akka.http.scaladsl.model.HttpMethods._
import edu.knuca.resmat.tests.TestConfsService
import edu.knuca.resmat.utils.S3Manager

import scala.concurrent.ExecutionContext

class HttpRoutes(usersService: UsersService,
                 val authService: AuthService,
                 val userExamService: UserExamService,
                 val examService: ExamService,
                 val testConfsService: TestConfsService,
                 val testSetExamService: TestSetExamService,
                 val problemService: ProblemService,
                 val s3Manager: S3Manager)
                (val dataGenerator: InitialDataGenerator)
                (implicit executionContext: ExecutionContext)
    extends ApiExceptionHandlers
    with ApiRejectionHandler
    with SecurityDirectives {

  val usersRouter = new UsersRoute(authService, usersService)
  val authRouter = new AuthRoute(authService, usersService)
  val studentsRouter = new StudentsRoute(usersService)
  val testConfsRoute = new TestConfsRoute(testConfsService)
  val testSetExamRouter = new TestSetExamRoute(userExamService)
  val taskFlowExamRouter = new TaskFlowExamRoute(userExamService)
  val examRouter = new ExamRoute(userExamService, testSetExamRouter, taskFlowExamRouter, s3Manager)
  val examConfRouter = new ExamConfRoute(examService)
  val problemConfRoute = new ProblemConfRoute(problemService)
  val tmpFileUploadRouter = new TmpFileUploadRoute(s3Manager)

  val corsSettings = CorsSettings.defaultSettings.copy(
    allowedMethods = scala.collection.immutable.Seq(GET, POST, PUT, DELETE, HEAD, OPTIONS)
  )

  val routes =
    pathPrefix("v1") {
      handleRejections(corsRejectionHandler) {
        cors(corsSettings) {
          (handleExceptions(generalHandler) & handleRejections(generalRejectionHandler)) {
            path("generate") {
              pathEndOrSingleSlash {
                get {
                  complete {
                    dataGenerator.generate()
                    "Ok"
                  }
                }
              }
            }
            authRouter.route ~
              authenticate { implicit user: AuthenticatedUser =>
                usersRouter.route ~
                  studentsRouter.route ~
                  examRouter.route ~
                  examConfRouter.route ~
                  problemConfRoute.route ~
                  testConfsRoute.route ~
                  tmpFileUploadRouter.route
              }
          }
        }
      }
    }

}
