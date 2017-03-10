package edu.knuca.resmat.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.directives.RouteDirectives._

import de.heikoseeberger.akkahttpcirce.CirceSupport
import io.circe.generic.auto._

object HttpUtils extends CirceSupport {

  def completeNotFound = complete(StatusCodes.NotFound -> ErrorMessage("Requested resource not found"))

  def completeForbidden(message: Any) =
    complete(StatusCodes.Forbidden -> ErrorMessage(s"Access to requested resource forbidden. $message", ErrorCodes.AuthorizationError))

  def completeUnauthorized(message: String = "") =
    complete(StatusCodes.Unauthorized -> ErrorMessage(s"Authentication failed. $message"))

}
