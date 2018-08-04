package edu.knuca.resmat.exam

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{authorize, _}
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.exam.taskflow.TaskFlowExamRoute
import edu.knuca.resmat.exam.testset.TestSetExamRoute
import edu.knuca.resmat.user.AuthenticatedUser
import io.circe.Json
import io.circe.generic.auto._

import scala.concurrent.{ExecutionContext, Future}

class ExamConfRoute(examService: ExamService)
                   (implicit executionContext: ExecutionContext) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._
  import examService._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = (pathPrefix("exam-confs") & authorize(user.isAssistantOrHigher)) {
    pathEndOrSingleSlash{
      get {
        complete {
          Future(findExamConfs())
        }
      } ~ (post & entity(as[ExamConfDto])) { examConfDto =>
        complete {
          Future(examService.createExamConfWithSteps(examConfDto))
        }
      }
    } ~
    (pathPrefix(LongNumber) & authorize(user.isAdmin)) { examConfId =>
      pathEndOrSingleSlash {
        get {
          complete {
            Future(getExamConf(examConfId))
          }
        } ~ (put & entity(as[ExamConfDto])) { examConfDto =>
          complete {
            Future(examService.updateExamConfWithSteps(examConfId, examConfDto))
          }
        }
      } ~
      pathPrefix("dto") {
        (pathEndOrSingleSlash & get) {
          complete {
            Future(getExamConfDto(examConfId))
          }
        }
      }
    }
  }

}
