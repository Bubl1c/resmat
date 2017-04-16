package edu.knuca.resmat.exam.testset

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.LongNumber
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.exam.UserExamService
import edu.knuca.resmat.user.AuthenticatedUser
import io.circe.generic.auto._

import scala.concurrent.{ExecutionContext, Future}

class TestSetExamRoute(examService: UserExamService) extends CirceSupport {

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
