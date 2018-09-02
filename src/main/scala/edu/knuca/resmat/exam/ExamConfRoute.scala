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

class ExamConfRoute(examConfService: ExamConfService)
                   (implicit executionContext: ExecutionContext) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._

  def route(implicit user: AuthenticatedUser): Route = (pathPrefix("exam-confs") & authorize(user.isAssistantOrHigher)) {
    pathEndOrSingleSlash{
      get {
        complete {
          Future(examConfService.findExamConfs())
        }
      } ~ (post & entity(as[ExamConfCreateDto])) { examConfDto =>
        complete {
          Future(examConfService.createExamConfWithSteps(examConfDto))
        }
      }
    } ~
    (pathPrefix(LongNumber) & authorize(user.isAdmin)) { examConfId =>
      pathEndOrSingleSlash {
        get {
          complete {
            Future(examConfService.getExamConf(examConfId))
          }
        } ~ (put & entity(as[ExamConfUpdateDto])) { examConfDto =>
          complete {
            Future(examConfService.updateExamConfWithSteps(examConfId, examConfDto))
          }
        } ~
        delete {
          complete {
            Future(examConfService.deleteExamConf(examConfId))
          }
        }
      } ~
      pathPrefix("dto") {
        (pathEndOrSingleSlash & get) {
          complete {
            Future(examConfService.getExamConfDto(examConfId))
          }
        }
      }
    }
  }

}
