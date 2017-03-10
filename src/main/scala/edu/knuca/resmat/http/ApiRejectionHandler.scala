package edu.knuca.resmat.http

import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import StatusCodes._
import Directives._

trait ApiRejectionHandler {
  implicit def generalRejectionHandler =
    RejectionHandler.newBuilder()
      .handle {
        case AuthenticationFailedRejection(cause, challenge) =>
          complete(HttpResponse(Unauthorized))
        case AuthorizationFailedRejection =>
          complete(HttpResponse(Unauthorized))
        case MissingHeaderRejection(header) =>
          complete(Unauthorized -> s"Header $header not found!")
      }
      .handleNotFound(complete(HttpResponse(NotFound)))
      .result()
}