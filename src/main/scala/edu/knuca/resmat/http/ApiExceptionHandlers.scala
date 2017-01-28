package edu.knuca.resmat.http

import akka.http.scaladsl.server._
import akka.http.scaladsl.server.directives.RouteDirectives
import com.typesafe.scalalogging.LazyLogging
import de.heikoseeberger.akkahttpcirce.CirceSupport
import io.circe.generic.auto._
import akka.http.scaladsl.model.StatusCodes

trait ApiExceptionHandlers extends RouteDirectives with CirceSupport with LazyLogging {

  val DateFormat = "MM-dd-yy 'at' hh:mma"

  implicit def generalHandler = ExceptionHandler {
    case CredentialsExpiredException(m) =>
      logger.warn("User credentials expired: ", m)
      complete(StatusCodes.Forbidden -> ErrorMessage("Credentials expired. Please change your password", ErrorCodes.CredentialsExpired))
    case e @ NotFoundException(m) =>
      logger.warn(s"Item not found: ", e)
      completeNotFound
    case NotAuthorized(m) =>
      logger.warn(s"User not authorized: ", m)
      completeNotFound
    case ValidationException(m) =>
      logger.warn(s"Entity validation failed: ", m)
      complete(StatusCodes.UnprocessableEntity -> ValidationError(m))
    case InvalidTokenException(m) =>
      logger.warn(s"Unprocessable token: ", m)
      complete(StatusCodes.BadRequest -> ErrorMessage(m, ErrorCodes.InvalidToken))
    case UnauthenticatedException(m) =>
      logger.warn(s"Unprocessable token: ", m)
      complete(StatusCodes.Unauthorized -> ErrorMessage(m))

    case e =>
      val id = java.util.UUID.randomUUID()
      logger.error(s"Server Error $id", e)
      complete(StatusCodes.InternalServerError -> ErrorMessage(s"There was an error processing your request. Reference id: $id"))
  }

  private def completeNotFound = complete(StatusCodes.NotFound -> ErrorMessage("Requested resource not found"))
}

trait ApiException

case class NotAuthorized(message: String) extends Exception(message) with ApiException

case class NotFoundException(message: String) extends Exception(message) with ApiException
case class ValidationException(messages: String) extends Exception(messages) with ApiException
case class CredentialsExpiredException(userId: Int) extends Exception(s"Credentials for user #$userId have expired") with ApiException

case class InvalidTokenException(message: String = "Failed to parse authentication token") extends Exception(message) with ApiException
case class UnauthenticatedException(message: String = "Authentication failed")  extends Exception(message) with ApiException

trait ApiResponse
case class ValidationError(message: String, errorCode: Int = ErrorCodes.ValidationError) extends ApiResponse
case class ErrorMessage(message: String, errorCode: Int = ErrorCodes.NotSpecified) extends ApiResponse

object ErrorCodes {
  val NotSpecified = 0

  val ValidationError = 10
  val MalformedRequest = 11

  val CredentialsExpired = 12

  val AuthorizationError = 20
  val InvalidToken = 30
}