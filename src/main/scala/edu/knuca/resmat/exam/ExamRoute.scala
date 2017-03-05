package edu.knuca.resmat.exam

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.circe.generic.auto._
import io.circe.syntax._
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.user.AuthenticatedUser

import scala.concurrent.{ExecutionContext, Future}

class ExamRoute(examService: ExamService)
               (implicit executionContext: ExecutionContext) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._
  import examService._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = pathPrefix("exams") {
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
    pathPrefix(LongNumber) { userExamId =>
      (pathEndOrSingleSlash & get) {
        complete {
          Future(getUserExamById(userExamId))
        }
      } ~
      pathPrefix("steps") {
        (pathEndOrSingleSlash & get) {
          complete{
            Future(getUserExamStepInfos(userExamId))
          }
        } ~
        pathPrefix(IntNumber) { sequence =>
          pathEndOrSingleSlash {
            get {
              complete(Future(getUserExamStepInfo(userExamId, sequence)))
            }
          } ~
          pathPrefix("attempt") {
            get {
              complete(Future(getUserExamStepCurrentAttempt(userExamId, sequence)).map(_.asJson))
            }
          }
        }
      }
    }
  }

}
