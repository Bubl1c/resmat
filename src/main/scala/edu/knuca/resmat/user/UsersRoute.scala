package edu.knuca.resmat.user

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.IntNumber
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.auth.AuthService
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.ExecutionContext

class UsersRoute(val authService: AuthService, usersService: UsersService)
                (implicit executionContext: ExecutionContext) extends CirceSupport {

  import StatusCodes._
  import usersService._

  import edu.knuca.resmat.http.JsonProtocol._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = pathPrefix("api-users") {
    pathPrefix("current") {
      pathEndOrSingleSlash {
        get {
          complete(getById(user.id))
        } ~
        put {
          entity(as[UserEntityUpdate]) { userUpdate =>
            complete(updateUser(user.id, userUpdate).map(_.asJson))
          }
        }
      }
    } ~
    pathPrefix("logout") {
      pathEndOrSingleSlash {
        post {
          complete {
            authService.logout(user)
            NoContent
          }
        }
      }
    } ~
    authorize(user.isInstructorOrHigher) {
      pathEndOrSingleSlash {
        get {
          complete(getAllNotStudents().map(_.asJson))
        } ~
        (post & authorize(user.isAdmin)) {
          entity(as[UserEntity]) { userEntity =>
            complete(Created -> createUser(userEntity).map(_.asJson))
          }
        }
      } ~
      pathPrefix(LongNumber) { id =>
        pathEndOrSingleSlash {
          get {
            complete(getById(id).map(_.asJson))
          } ~
          (put & authorize(user.isAdmin)) {
            entity(as[UserEntityUpdate]) { userUpdate =>
              complete(updateUser(id, userUpdate).map(_.asJson))
            }
          } ~
          (delete & authorize(user.isAdmin)) {
            onSuccess(deleteUser(id)) { ignored =>
              complete(NoContent)
            }
          }
        }
      }
    }
  }

}
