package edu.knuca.resmat.auth

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.tests.TestConfsService
import edu.knuca.resmat.user.AuthenticatedUser
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}

class TestConfsRoute(val testConfsService: TestConfsService) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route =
    (pathPrefix("test-groups") & authorize(user.isAdmin)) {
      pathEndOrSingleSlash {
        get {
          complete(Future(testConfsService.getTestGroupConfs()))
        }
      } ~
      pathPrefix(LongNumber) { testGroupConfId =>
        pathPrefix("tests") {
          pathEndOrSingleSlash {
            get {
              complete(testConfsService.findTestConfsByGroup(testGroupConfId))
            }
          }
        }
      }
    }

}
