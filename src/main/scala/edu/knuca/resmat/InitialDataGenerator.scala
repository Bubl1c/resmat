package edu.knuca.resmat

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.auth.{AuthService, TokenEntity, TokensQueries}
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.user.{UserEntity, UserGroupEntity, UserType, UsersService}
import org.joda.time.DateTime

import scala.concurrent.duration.
import scala.concurrent.{Await, Awaitable, ExecutionContext}

object Data {
  val group1 = UserGroupEntity(None, "group1")

  def userStudent(goupId: Long) = UserEntity(None, "student", "root", "Andrii student", "Mozharovskyi", "student@email.com", UserType.Student, Some(goupId))
  val userAdmin = UserEntity(None, "admin", "root", "Andrii admin", "Mozharovskyi", "admin@email.com", UserType.Admin, None)
  val userInstructor = UserEntity(None, "instructor", "root", "Andrii instructor", "Mozharovskyi", "instructor@email.com", UserType.Instructor, None)

  def userToken(userId: Long) =
    TokenEntity(None, userId, "b1d3981def9b427bbf9707eedbdc0cbe", DateTime.parse("2017-01-27T18:37:24.000+02:00").plusMinutes(userId.toInt), Some(DateTime.now().plusYears(100)))
}

class InitialDataGenerator(db: DatabaseService, usersService: UsersService, authService: AuthService) extends LazyLogging {

  def generate()(implicit executionContext: ExecutionContext) = {

    val group = await(usersService.createGroup(Data.group1))

    val student = await(usersService.createUser(Data.userStudent(group.id.get)))
    val admin = await(usersService.createUser(Data.userAdmin))
    val instructor = await(usersService.createUser(Data.userInstructor))

    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYxJjE0ODU1MzUxMDQwMDA
    val studentToken = insertToken(Data.userToken(student.id.get))
    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYyJjE0ODU1MzUxNjQwMDA
    val adminToken = insertToken(Data.userToken(admin.id.get))
    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYzJjE0ODU1MzUyMjQwMDA
    val instructorToken = insertToken(Data.userToken(instructor.id.get))
  }

  def insertToken(token: TokenEntity): TokenEntity = {
    db.run{ implicit c =>
      val createdId: Option[Long] = TokensQueries.insert(token.userId, token.token, token.created, token.expires).executeInsert()
      token.copy(id = createdId)
    }
  }

  def await[T](awaitable: Awaitable[T]): T = Await.result(awaitable, 5 seconds)

}
