package edu.knuca.resmat.user

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import edu.knuca.resmat.utils.{FileUploadUtils, S3Manager}

import scala.concurrent.ExecutionContext

class TmpFileUploadRoute(s3Manager: S3Manager) {
  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = {
    pathPrefix("upload-temp-file") {
      FileUploadUtils.toS3TmpFileUpload(s3Manager, user.id)
    }
  }
}
