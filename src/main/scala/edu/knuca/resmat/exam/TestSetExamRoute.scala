package edu.knuca.resmat.exam

import akka.http.scaladsl.server.PathMatchers.LongNumber
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import io.circe.generic.auto._
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.user.AuthenticatedUser

import scala.concurrent.{ExecutionContext, Future}

class TestSetExamRoute(examService: ExamService) extends CirceSupport {

  def route(userExamId: Long, stepSequence: Int, attemptId: Long)
           (implicit user: AuthenticatedUser, ec: ExecutionContext): Route =
    pathPrefix("tests" / LongNumber) { testId =>
      pathPrefix("verify") {
        pathEndOrSingleSlash {
          (post & entity(as[Seq[Long]])) { submittedOptions =>
            complete(Future(examService.verifyTestSetAnswer(userExamId, stepSequence, attemptId, testId, submittedOptions)))
          }
        }
      }
    }

}
