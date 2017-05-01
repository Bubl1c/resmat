package edu.knuca.resmat.auth

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.data.InitialDataGenerator
import edu.knuca.resmat.http.SecurityDirectives
import edu.knuca.resmat.user.{UserEntity, UsersService}
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.ExecutionContext

class AuthRoute(val authService: AuthService, val usersService: UsersService)
               (implicit executionContext: ExecutionContext) extends CirceSupport with SecurityDirectives {

  import StatusCodes._
  import authService._
  import edu.knuca.resmat.http.JsonProtocol._

  val route = pathPrefix("auth") {
    pathEndOrSingleSlash {
      get {
        complete("auth get result")
      }
    } ~
    path("sign-in") {
      pathEndOrSingleSlash {
        post {
          entity(as[LoginPassword]) { loginPassword =>
            complete(signIn(loginPassword.login, loginPassword.password))
          }
        }
      }
    } ~
    path("access-key") {
      pathEndOrSingleSlash {
        post {
          entity(as[AccessKey]) { accessKey =>
            complete(signInWithAccessKey(accessKey.accessKey).map(_.asJson))
          }
        }
      }
    }
  }

  private case class LoginPassword(login: String, password: String)
  private case class AccessKey(accessKey: String)

}
