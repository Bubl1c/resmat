package edu.knuca.resmat.utils

import akka.http.scaladsl.server.Directives.{complete, fileUpload}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.BasicDirectives.{extractRequestContext}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Try}

object FileUploadUtils {

  def toS3FileUpload(s3Manager: S3Manager, folder: String, overrideFileName: Option[String] = None, isTemp: Boolean = true)
                    (implicit ec: ExecutionContext): Route = {
    extractRequestContext { ctx ⇒
      implicit val mat = ctx.materializer
      val fileSizeHeader = ctx.request.headers.find(_.lowercaseName() == "file-size")
      if(fileSizeHeader.isEmpty) {
        complete(400, "'file-size' header must be provided")
      } else {
        val fileSizeBytesTry = Try { fileSizeHeader.get.value().toLong }
        fileSizeBytesTry match {
          case scala.util.Success(fileSizeBytes) =>
            fileUpload("file") {
              case (fileInfo, fileSource) =>
                complete(s3Manager.put(folder, overrideFileName.getOrElse(fileInfo.fileName), fileSource, fileSizeBytes, isTemp))
              case _ => complete(400, "Unsupported file upload")
            }
          case Failure(t) => complete(400, "Value of 'file-size' header must be a positive number. Error: " + t.getMessage)
        }
      }
    }
  }

}
