package edu.knuca.resmat.http

import akka.http.scaladsl.server.directives.{BasicDirectives, FutureDirectives, HeaderDirectives, RouteDirectives}
import akka.http.scaladsl.server.{Directive1, MissingHeaderRejection}
import edu.knuca.resmat.auth.{AuthService, TokenUtils}
import edu.knuca.resmat.user.AuthenticatedUser

import scala.util.{Failure, Success}

trait SecurityDirectives {

  import BasicDirectives._
  import HeaderDirectives._
  import RouteDirectives._
  import FutureDirectives._
  import HttpUtils._

  val authorizationTokenName = "Token"

  protected val authService: AuthService

  //todo change complete methods to Exception. Had to use complete as when exception thrown and parsed in exception handler -
  // browser couldn't see response data and i couldn't parse status code in angular
  def authenticate: Directive1[AuthenticatedUser] = {
    optionalHeaderValueByName(authorizationTokenName).flatMap {
      case Some(token) =>
        val decodedTokenOpt = TokenUtils.parse(token)
        decodedTokenOpt match {
          case Some(decodedToken) =>
            onComplete(authService.authenticate(decodedToken)).flatMap {
              case Success(Some(user)) => provide(user)
              case Failure(t) => completeUnauthorized(t.getMessage)
              case _ => completeUnauthorized()
            }
          case None => completeUnauthorized("Invalid token")
        }
      case None => completeUnauthorized("Token not found")
    }
  }

}
