package edu.knuca.resmat.auth

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.exam.{TestConf, TestGroupConf}
import edu.knuca.resmat.tests.TestConfsService
import edu.knuca.resmat.user.AuthenticatedUser
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}

class TestConfsRoute(val testConfsService: TestConfsService) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route =
    (pathPrefix("test-set-confs") & authorize(user.isInstructorOrHigher)) {
      pathPrefix(LongNumber) { testSetConfId =>
        complete(testConfsService.getTestSetConfDto(testSetConfId))
      }
    } ~
    (pathPrefix("test-groups") & authorize(user.isInstructorOrHigher)) {
      pathEndOrSingleSlash {
        (post & entity(as[TestGroupConf])) { testGroupConf =>
          complete(Future(testConfsService.createTestGroupConf(testGroupConf)))
        } ~
        get {
          complete(Future(testConfsService.getTestGroupConfs()))
        }
      } ~
      pathPrefix(LongNumber) { testGroupConfId =>
        pathEndOrSingleSlash {
          (put & entity(as[TestGroupConf])) { testGroupConf =>
            complete(Future(testConfsService.editTestGroupConf(testGroupConfId, testGroupConf)))
          }
        } ~
        pathPrefix("tests") {
          pathEndOrSingleSlash {
            (post & entity(as[TestConf])) { testConf =>
              complete(Future(testConfsService.createTestConf(testConf)))
            } ~
            get {
              complete(Future(testConfsService.findTestConfsByGroup(testGroupConfId)))
            }
          } ~
          pathPrefix(LongNumber) { testConfId =>
            (put & entity(as[TestConf])) { testConf =>
              complete(Future(testConfsService.editTestConf(testConfId, testConf)))
            } ~
            delete {
              complete(204, Future(testConfsService.deleteTestConf(testConfId)))
            }
          }
        }
      }
    }

}
