package edu.knuca.resmat.exam

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.LongNumber
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.user.AuthenticatedUser
import io.circe.generic.auto._

import scala.concurrent.{ExecutionContext, Future}

class TaskFlowExamRoute(examService: ExamService) extends CirceSupport {

  def route(userExamId: Long, stepSequence: Int, attemptId: Long)
           (implicit user: AuthenticatedUser, ec: ExecutionContext): Route =
    pathPrefix("task-flows" / LongNumber) { taskFlowIdId =>
      pathPrefix("steps" / LongNumber) { taskFlowStepId =>
        pathPrefix("verify") {
          pathEndOrSingleSlash {
            (post & entity(as[String])) { answer =>
              complete(Future(
                examService.verifyTaskFlowStepAnswer(
                  userExamId, stepSequence, attemptId, taskFlowIdId, taskFlowStepId, answer)
              ))
            }
          }
        }
      }
    }

}
