package edu.knuca.resmat.auth

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.exam.{TestConf, TestGroupConf, UserTestGroupAccessDto}
import edu.knuca.resmat.tests.TestConfService
import edu.knuca.resmat.user.AuthenticatedUser
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}

class TestConfsRoute(val testConfsService: TestConfService) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route =
    (pathPrefix("test-set-confs") & authorize(user.isInstructorOrHigher)) {
      pathPrefix(LongNumber) { testSetConfId =>
        complete(testConfsService.getTestSetConfDto(testSetConfId))
      }
    } ~
    (pathPrefix("test-groups") & authorize(user.isAssistantOrHigher)) {
      pathEndOrSingleSlash {
        (post & entity(as[TestGroupConf])) { testGroupConf =>
          complete(Future(testConfsService.createTestGroupConf(testGroupConf, Some(user.id))))
        } ~
        (parameters('isArchived.as[Boolean].?) & get) { isArchived =>
          complete(Future(testConfsService.getTestGroupConfs(isArchived, onlyAccessible = !user.isAdmin)))
        }
      } ~
      (pathPrefix("access") & authorize(user.isAdmin)) {
        pathEndOrSingleSlash {
          (parameters('userId.as[Long]) & get) { userId =>
            complete {
              Future(testConfsService.getAccessToTestGroups(userId))
            }
          } ~
            (put & entity(as[UserTestGroupAccessDto])) { accessDto =>
              complete {
                Future(testConfsService.setUserTestGroupAccess(accessDto))
              }
            }
        }
      } ~
      pathPrefix("with-amount-of-tests") {
        pathEndOrSingleSlash {
          get {
            complete(Future(testConfsService.getTestGroupConfsWithAmountOfTests()))
          }
        }
      } ~
      pathPrefix(LongNumber) { testGroupConfId =>
        pathEndOrSingleSlash {
          (put & entity(as[TestGroupConf])) { testGroupConf =>
            complete(Future(testConfsService.updateTestGroupConf(testGroupConfId, testGroupConf)))
          }
        } ~
        pathPrefix("tests") {
          pathEndOrSingleSlash {
            (post & entity(as[TestConf])) { testConf =>
              complete(Future(testConfsService.createTestConf(testConf)))
            } ~
            get {
              complete(Future(testConfsService.findTestConfsByGroup(testGroupConfId)))
            } ~
            (put & entity(as[Seq[TestConf]])) { testConfs =>
              complete(Future(testConfsService.bulkSetGroupTestConfs(testGroupConfId, testConfs)))
            }
          } ~
          pathPrefix(LongNumber) { testConfId =>
            (put & entity(as[TestConf])) { testConf =>
              complete(Future(testConfsService.updateTestConf(testConfId, testConf)))
            } ~
            delete {
              complete(204, Future(testConfsService.deleteTestConf(testConfId)))
            }
          }
        }
      }
    }

}
