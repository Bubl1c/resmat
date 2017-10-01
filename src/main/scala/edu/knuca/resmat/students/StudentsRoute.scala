package edu.knuca.resmat.students

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{authorize, _}
import akka.http.scaladsl.server.PathMatchers.IntNumber
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.CirceSupport
import edu.knuca.resmat.auth.AuthService
import edu.knuca.resmat.user._
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}

class StudentsRoute(usersService: UsersService)
                   (implicit executionContext: ExecutionContext) extends CirceSupport {

  import StatusCodes._
  import usersService._

  import edu.knuca.resmat.http.JsonProtocol._

  def route(implicit user: AuthenticatedUser, ec: ExecutionContext): Route = (pathPrefix("student-groups") & authorize(user.notStudent)) {
    pathEndOrSingleSlash {
      get {
        complete(getAllStudentGroups().map(_.asJson))
      } ~
      post {
        entity(as[StudentGroupEntity]) { userGroupEntity =>
          complete(createStudentGroup(userGroupEntity).map(_.asJson))
        }
      }
    } ~
    pathPrefix(LongNumber) { studentGroupId =>
      pathEndOrSingleSlash {
        get {
          complete(getStudentGroupById(studentGroupId).map(_.asJson))
        } ~
        put {
          entity(as[StudentGroupEntityUpdate]) { groupUpdate =>
            complete(updateStudentGroup(studentGroupId, groupUpdate).map(_.asJson))
          }
        }
      } ~
      pathPrefix("articles") {
        pathEndOrSingleSlash {
          (put & entity(as[Seq[Long]])) { articleIds =>
            complete(Future(setArticlesToGroup(studentGroupId, articleIds)))
          } ~
          delete {
            complete(Future(setArticlesToGroup(studentGroupId, Seq())))
          }
        }
      } ~
      pathPrefix("students") {
        pathEndOrSingleSlash {
          get {
            complete(getStudentsByGroup(studentGroupId).map(_.asJson))
          } ~
          post {
            entity(as[UserEntity]) { userEntity =>
              require(userEntity.userType == UserType.Student, "Cannot add not student to the student group")
              complete(Created -> createUser(userEntity).map(_.asJson))
            }
          }
        } ~
        pathPrefix(LongNumber) { studentId =>
          pathEndOrSingleSlash {
            get {
              complete(getByIdInStudentGroup(studentId, studentGroupId))
            } ~
            delete {
              onSuccess(deleteUser(studentId, Some(UserType.Student))) { ignored =>
                complete(NoContent)
              }
            }
          } ~
          path("move") {
            post {
              entity(as[MoveStudentToAnotherGroup]) { moveToGroup =>
                complete(moveStudentToGroup(studentId, moveToGroup.groupId))
              }
            }
          }
        }
      }
    }
  }

  case class MoveStudentToAnotherGroup(groupId: Long)

}

