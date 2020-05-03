package edu.knuca.resmat.exam

import akka.http.scaladsl.server.Directives.{authorize, _}
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.user.AuthenticatedUser
import io.circe.generic.auto._

import scala.concurrent.{ExecutionContext, Future}

class ProblemConfRoute(problemService: ProblemConfService)
                      (implicit executionContext: ExecutionContext) extends CirceSupport {

  import edu.knuca.resmat.http.JsonProtocol._
  import problemService._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = (pathPrefix("problem-confs") & authorize(user.isInstructorOrHigher)) {
    pathEndOrSingleSlash{
      get {
        complete {
          Future(findProblemConfs())
        }
      }
    } ~
    pathPrefix(LongNumber) { problemConfId =>
      (pathEndOrSingleSlash & get) {
        complete {
          Future(getProblemConfById(problemConfId))
        }
      } ~
      pathPrefix("variants") {
        (pathEndOrSingleSlash & post) {
          entity(as[NewProblemVariantConfDto]) { dto =>
            complete(calculateAndCreateProblemVariantConf(dto, problemConfId))
          }
        } ~
        pathPrefix(LongNumber) { problemVariantConfId =>
          pathEndOrSingleSlash {
            (delete & parameters('force ? false)) { force =>
              complete {
                Future(deleteProblemVariantConf(problemVariantConfId, force))
              }
            }
          }
        }
      } ~
      (pathPrefix("recalculate-variants") & authorize(user.isAdmin)) {
        pathEndOrSingleSlash {
          put {
            complete(Future(recalculateProblemVariantConfs(problemConfId)))
          }
        }
      } ~
      pathPrefix("with-variants") {
        (pathEndOrSingleSlash & get) {
          complete{
            Future(getProblemConfWithVariants(problemConfId))
          }
        }
      }
    }
  }

}
