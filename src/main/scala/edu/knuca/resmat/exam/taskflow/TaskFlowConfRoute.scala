package edu.knuca.resmat.exam.taskflow

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.LongNumber
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.user.AuthenticatedUser
import io.circe.generic.auto._

import scala.concurrent.{ExecutionContext, Future}

class TaskFlowConfRoute(taskFlowConfAndExamService: TaskFlowConfAndExamService) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route =
    pathPrefix("task-flow-confs" / LongNumber) { taskFlowConfId =>
      pathEndOrSingleSlash {
        get {
          complete(Future(taskFlowConfAndExamService.getTaskFlowConfDto(taskFlowConfId)))
        }
      }
    }

}