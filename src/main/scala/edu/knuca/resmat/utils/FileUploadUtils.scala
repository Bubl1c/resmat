package edu.knuca.resmat.utils

import java.io.InputStream
import java.util.concurrent.TimeUnit

import akka.http.scaladsl.server.Directives.{complete, fileUpload}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.BasicDirectives.extractRequestContext
import akka.stream.Materializer
import akka.stream.scaladsl.StreamConverters
import akka.util.ByteString

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

object FileUploadUtils {

  def toS3TmpFileUpload(s3Manager: S3Manager, userId: Long)(implicit ec: ExecutionContext): Route = {
    toS3FileUpload(s3Manager, S3Manager.userTmpFolder(userId), (fileName) => S3Manager.makeTmpFileName(fileName))
  }

  def toS3FileUpload(s3Manager: S3Manager, folder: String, overrideFileName: String => String = (n) => n)
                    (implicit ec: ExecutionContext): Route = {
    extractRequestContext { ctx ⇒
      implicit val mat = ctx.materializer
      val fileSizeHeader = ctx.request.headers.find(_.lowercaseName() == "file-size")
      if (fileSizeHeader.isEmpty) {
        complete(400, "'file-size' header must be provided")
      } else {
        val fileSizeBytesTry = Try {
          fileSizeHeader.get.value().toLong
        }
        fileSizeBytesTry match {
          case scala.util.Success(fileSizeBytes) =>
            fileUpload("file") {
              case (fileInfo, fileSource) =>
                complete(uploadStream(s3Manager, folder, overrideFileName(fileInfo.fileName), fileSource, fileSizeBytes))
              case _ => complete(400, "Unsupported file upload")
            }
          case Failure(t) => complete(400, "Value of 'file-size' header must be a positive number. Error: " + t.getMessage)
        }
      }
    }
  }

  private def uploadStream(s3Manager: S3Manager,
                   folder: String,
                   fileName: String,
                   source: akka.stream.scaladsl.Source[ByteString, Any],
                   sizeBytes: Long)
                  (implicit ec: ExecutionContext, mat: Materializer): Future[Try[String]] = Future {
    val inputStream: InputStream = source.runWith(
      StreamConverters.asInputStream(FiniteDuration(10, TimeUnit.SECONDS))
    )
    s3Manager.put(
      PathUtils.normalisePath(folder, "", "/") + PathUtils.normalisePath(fileName),
      inputStream,
      sizeBytes
    ).map(s3Manager.baseUrl + _)
  }

}
