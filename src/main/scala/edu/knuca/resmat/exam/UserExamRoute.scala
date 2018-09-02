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
import edu.knuca.resmat.utils.{FileUploadUtils, S3Manager}

import scala.concurrent.{ExecutionContext, Future}

class UserExamRoute(examService: UserExamService, testSetExamRoute: TestSetExamRoute, taskFlowExamRoute: TaskFlowExamRoute, s3Manager: S3Manager)
                   (implicit executionContext: ExecutionContext) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._
  import examService._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = pathPrefix("user-exams") {
    pathEndOrSingleSlash{
      (parameters('userId.as[Long].?) & get) { userId =>
        complete {
          Future(findUserExamsAvailableForUser(userId.getOrElse(user.id)))
        }
      } ~
      (parameters('userId.as[Long], 'examConfId.as[Long]) & post & authorize(user.isAssistantOrHigher)) { (userId, examConfId) =>
        complete {
          Future(createUserExam(userId, examConfId))
        }
      }
    } ~
    (pathPrefix("upload") & authorize(user.isAssistantOrHigher)) {
      FileUploadUtils.toS3TmpFileUpload(s3Manager, user.id)
    } ~
    (pathPrefix("lockAll") & authorize(user.isAssistantOrHigher)) {
      (parameters('groupId.as[Long], 'hoursAmount.as[Int]) & put) { (groupId, hoursAmount) =>
        complete {
          Future(lockAllForGroup(groupId, hoursAmount))
        }
      }
    } ~
    (pathPrefix("unlockAll") & authorize(user.isAssistantOrHigher)) {
      (parameters('groupId.as[Long]) & put) { groupId =>
        complete {
          Future(unlockAllForGroup(groupId))
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
        Future(findUserExamResults(userId))
      }
    } ~
    pathPrefix(LongNumber) { userExamId =>
      pathEndOrSingleSlash {
        get {
          complete {
            Future(getUserExamDto(userExamId))
          }
        } ~
        (delete & authorize(user.isInstructorOrHigher)) {
          complete(Future(deleteUserExam(userExamId)))
        }
      } ~
      (pathPrefix("unlock") & authorize(user.isAssistantOrHigher)) {
        put {
          complete{
            Future(unlockUserExam(userExamId))
          }
        }
      } ~
      (pathPrefix("lock") & authorize(user.isAssistantOrHigher)) {
        (parameters('hoursAmount.as[Int]) & put) { hoursAmount =>
          complete{
            Future(lockUserExam(userExamId, hoursAmount))
          }
        }
      } ~
      pathPrefix("start") {
        (pathEndOrSingleSlash & get) {
          complete{
            Future(startAndGetUserExamDto(userExamId))
          }
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
          }
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
