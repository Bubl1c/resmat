package edu.knuca.resmat.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.directives.RouteDirectives
import com.typesafe.scalalogging.LazyLogging
import de.heikoseeberger.akkahttpcirce.CirceSupport
import io.circe.generic.auto._
import akka.http.scaladsl.model._
import edu.knuca.resmat.GeneralHelpers

trait ApiExceptionHandlers extends RouteDirectives with CirceSupport with LazyLogging {

  val DateFormat = "MM-dd-yy 'at' hh:mma"

  import HttpUtils._

  def generalHandler = ExceptionHandler {
    case NotFoundException(m) =>
      logger.warn(s"Item not found: ", m)
      completeNotFound
    case NotAuthorized(m) =>
      logger.warn(s"User not authorized: ", m)
      completeForbidden(m)
    case InvalidTokenException(m) =>
      logger.warn(s"Unprocessable token: ", m)
      completeUnauthorized(m)
    case TokenNotFoundException(m) =>
      logger.warn(s"Token not found: ", m)
      completeUnauthorized(m)
    case UnauthenticatedException(m) =>
      logger.warn(s"Authentication failed: ", m)
      completeUnauthorized(m)
    case ResourceLocked(lockedUntil, message) =>
      complete(StatusCodes.Locked -> lockedUntil.toString())
    case e: IllegalStateException =>
      complete(StatusCodes.Conflict -> ErrorMessage(e.getMessage))

    case e =>
      val id = java.util.UUID.randomUUID()
      logger.error(s"Server Error $id", e)
      complete(StatusCodes.InternalServerError -> ErrorMessage(s"There was an error processing your request: ${e.getMessage}. Reference id: $id"))
  }
}

trait ApiException

case class NotAuthorized(message: String = "Access to this resource is forbidden") extends Exception(message) with ApiException

case class ResourceLocked(lockedUntil: org.joda.time.DateTime, message: String = "Resource is locked.") extends Exception(message) with ApiException

case class NotFoundException(message: String) extends Exception(message) with ApiException

case class TokenNotFoundException(message: String = "Authorization token not found") extends Exception(message) with ApiException
case class InvalidTokenException(message: String = "Failed to parse authorization token") extends Exception(message) with ApiException
case class UnauthenticatedException(message: String = "Authentication failed")  extends Exception(message) with ApiException

trait ApiResponse
case class ErrorMessage(message: String, errorCode: Int = ErrorCodes.NotSpecified) extends ApiResponse

object ErrorCodes {
  val NotSpecified = 0

  val MalformedRequest = 5

  val AuthorizationError = 10
  val InvalidToken = 20
  val TokenNotFound = 30
}