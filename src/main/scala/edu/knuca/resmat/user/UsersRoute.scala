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

class UsersRoute(val authService: AuthService,
                 usersService: UsersService
                       )(implicit executionContext: ExecutionContext) extends CirceSupport {

  import StatusCodes._
  import usersService._

  import edu.knuca.resmat.http.JsonProtocol._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = pathPrefix("users") {
    pathPrefix("current") {
      pathEndOrSingleSlash {
        get {
          complete(user)
        } ~
        put {
          entity(as[UserEntityUpdate]) { userUpdate =>
            complete(updateUser(user.id, userUpdate).map(_.asJson))
          }
        }
      }
    } ~
    authorize(user.notStudent) {
      pathEndOrSingleSlash {
        get {
          complete(getAll().map(_.asJson))
        }
        post {
          entity(as[UserEntity]) { userEntity =>
            complete(Created -> createUser(userEntity).map(_.asJson))
          }
        }
      } ~
        pathPrefix(IntNumber) { id =>
          pathEndOrSingleSlash {
            get {
              complete(getUserById(id).map(_.asJson))
            } ~
              put {
                entity(as[UserEntityUpdate]) { userUpdate =>
                  complete(updateUser(id, userUpdate).map(_.asJson))
                }
              } ~
              delete {
                onSuccess(deleteUser(id)) { ignored =>
                  complete(NoContent)
                }
              }
          }
        }
    }
  }

}
