package edu.knuca.resmat.http

import akka.http.scaladsl.server.directives.{BasicDirectives, FutureDirectives, HeaderDirectives, RouteDirectives}
import akka.http.scaladsl.server.Directive1
import edu.knuca.resmat.auth.{AuthService, TokenUtils}
import edu.knuca.resmat.user.AuthenticatedUser

trait SecurityDirectives {

  import BasicDirectives._
  import HeaderDirectives._
  import RouteDirectives._
  import FutureDirectives._

  def authenticate: Directive1[AuthenticatedUser] = {
    headerValueByName("Token").flatMap { token =>
      val decodedTokenOpt = TokenUtils.parse(token)
      decodedTokenOpt match {
        case Some(decodedToken) =>
          onSuccess(authService.authenticate(decodedToken)).flatMap {
            case Some(user) => provide(user)
            case None => throw UnauthenticatedException()
          }
        case None => throw InvalidTokenException()
      }
    }
  }



  protected val authService: AuthService

}
