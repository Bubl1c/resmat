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
        (get & parameters('own ? false, 'onlyVisible ? false, 'studentGroupId ? -1)) { (own, onlyVisible, studentGroupId) =>
          authorize(if(onlyVisible || own) true else user.isAdmin) {
            if(own) {
              if(user.userGroupId.isEmpty) {
                throw new IllegalStateException("Cannot load articles as user doesn't belong to any student group")
              } else {
                complete(Future(articleService.getByStudentGroupId(user.userGroupId.get, true)))
              }
            } else {
              if(studentGroupId > 0)
                complete(Future(articleService.getByStudentGroupId(studentGroupId, onlyVisible)))
              else
                complete(Future(articleService.get(onlyVisible)))
            }
          }
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
          } ~
          delete {
            complete(Future(articleService.delete(articleId)))
          }
        } ~
        pathPrefix("visible") {
          pathEndOrSingleSlash {
            get {
              complete(Future(articleService.getById(articleId, !user.isAdmin)))
            }
          }
        } ~
        (pathPrefix("upload-file") & authorize(user.isAdmin)) {
          FileUploadUtils.toS3FileUpload(s3Manager, s"articles/$articleId")
        }
      }
    }

  def publicRoute(implicit ec: ExecutionContext): Route =
    pathPrefix("public-articles") {
      (pathEndOrSingleSlash & get) {
        complete(Future(articleService.get(onlyIfVisible = true)))
      } ~
      pathPrefix(LongNumber) { articleId =>
        (pathEndOrSingleSlash & get) {
          complete(Future(articleService.getById(articleId, onlyIfVisible = true)))
        }
      }
    }

}
