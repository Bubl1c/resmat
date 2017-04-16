package edu.knuca.resmat.exam

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.circe.generic.auto._
import io.circe.syntax._
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.exam.taskflow.TaskFlowExamRoute
import edu.knuca.resmat.exam.testset.TestSetExamRoute
import edu.knuca.resmat.user.AuthenticatedUser

import scala.concurrent.{ExecutionContext, Future}

class ExamRoute(examService: UserExamService, testSetExamRoute: TestSetExamRoute, taskFlowExamRoute: TaskFlowExamRoute)
               (implicit executionContext: ExecutionContext) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._
  import examService._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = pathPrefix("user-exams") {
    pathEndOrSingleSlash{
      get {
        complete {
          Future(getUserExamsAvailableForUser(user.id))
        }
      }
    } ~
    (pathPrefix("current") & get) {
      complete {
        Future(getCurrentUserExam(user.id))
      }
    } ~
    (pathPrefix("results") & parameters("userId".as[Long]) & get) { userId =>
      complete {
        Future(getUserExamResults(userId))
      }
    } ~
    pathPrefix(LongNumber) { userExamId =>
      (pathEndOrSingleSlash & get) {
        complete {
          Future(getUserExamDtoById(userExamId))
        }
      } ~
      pathPrefix("steps") {
        (pathEndOrSingleSlash & get) {
          complete{
            Future(getUserExamStepInfos(userExamId))
          }
        } ~
        pathPrefix("current") {
          pathEndOrSingleSlash {
            get {
              complete{
                Future(getUserExamCurrentStepWithAttemptData(userExamId))
              }
            }
          } /*~
          pathPrefix("submit") {
            pathEndOrSingleSlash {
              (post & entity(as[])
            }
          }*/
        } ~
        pathPrefix(IntNumber) { stepSequence =>
          pathEndOrSingleSlash {
            get {
              complete(Future(getUserExamStepInfo(userExamId, stepSequence)))
            }
          } ~
          pathPrefix("submit") {
            pathEndOrSingleSlash {
              get {
                complete(Future(submitStep(userExamId, stepSequence)).map{ result =>
                  if(result) {
                    StatusCodes.NoContent
                  } else {
                    StatusCodes.Conflict
                  }
                })
              }
            }
          } ~
          pathPrefix("attempts") {
            pathEndOrSingleSlash {
              get {
                complete(Future(getUserExamStepAttempts(userExamId, stepSequence)))
              }
            } ~
            pathPrefix("current") {
              get {
                complete(Future(getUserExamStepCurrentAttempt(userExamId, stepSequence)))
              }
            } ~
            pathPrefix(LongNumber) { attemptId =>
              testSetExamRoute.route(userExamId, stepSequence, attemptId) ~
              taskFlowExamRoute.route(userExamId, stepSequence, attemptId)
            }
          }
        }
      }
    }
  }

}
