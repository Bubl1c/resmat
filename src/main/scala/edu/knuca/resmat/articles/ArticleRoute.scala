package edu.knuca.resmat.articles

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.LongNumber
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.user.AuthenticatedUser
import edu.knuca.resmat.utils.{FileUploadUtils, S3Manager}
import io.circe.generic.auto._

import scala.concurrent.{ExecutionContext, Future}

class ArticleRoute(articleService: ArticleService, s3Manager: S3Manager) extends CirceSupport {

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route =
    pathPrefix("articles") {
      pathEndOrSingleSlash {
        get {
          complete(Future(articleService.get()))
        } ~
        (post & entity(as[ArticleDto]) & authorize(user.isAdmin)) { articleToCreate =>
          complete(Future(articleService.create(articleToCreate)))
        }
      } ~
      pathPrefix(LongNumber) { articleId =>
        (pathEndOrSingleSlash & authorize(user.isAdmin)) {
          get {
            complete(Future(articleService.getById(articleId)))
          } ~
          (put & entity(as[ArticleDto])) { articleToUpdate =>
            complete(Future(articleService.update(articleId, articleToUpdate)))
          }
        } ~
        pathPrefix("visible") {
          pathEndOrSingleSlash {
            get {
              complete(Future(articleService.getById(articleId, !user.isAdmin)))
            }
          }
        } ~
        pathPrefix("upload-file") {
          FileUploadUtils.toS3FileUpload(s3Manager, s"articles/$articleId")
        }
      }
    }

  def publicRoute(implicit ec: ExecutionContext): Route =
    pathPrefix("public-articles") {
      (pathEndOrSingleSlash & get) {
        complete(Future(articleService.getVisible()))
      } ~
      pathPrefix(LongNumber) { articleId =>
        (pathEndOrSingleSlash & get) {
          complete(Future(articleService.getById(articleId, true)))
        }
      }
    }

}
