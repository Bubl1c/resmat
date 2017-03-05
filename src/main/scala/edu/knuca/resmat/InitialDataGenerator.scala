package edu.knuca.resmat

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.auth.{AuthService, TokenEntity, TokensQueries}
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.user.{UserEntity, StudentGroupEntity, UserType, UsersService}
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable, ExecutionContext}

object Data {
  val group1 = StudentGroupEntity(None, "ІП-41М")
  val group2 = StudentGroupEntity(None, "ІО-41")

  val userAdmin = UserEntity(None, "admin", "root", "Андрій", "Можаровський", "admin@email.com", UserType.Admin, "admin", None)
  val userInstructor = UserEntity(None, "instructor", "root", "Дмитро", "Левківський", "instructor@email.com", UserType.Instructor, "instructor", None)

  def userToken(userId: Long) =
    TokenEntity(None, userId, "3702dd845e4642659a3e7c930bc0fd37", DateTime.parse("2017-01-27T18:37:24.000+02:00").plusMinutes(userId.toInt), Some(DateTime.now().plusYears(100)))

  def student(goupId: Long, username: String, name: String, accessKey: String) =
    UserEntity(None, username, "root", name.split(" ")(0), name.split(" ")(1), s"$username@email.com", UserType.Student, accessKey, Some(goupId))
}

class InitialDataGenerator(db: DatabaseService, usersService: UsersService, authService: AuthService) extends LazyLogging {

  def generate()(implicit executionContext: ExecutionContext) = {

    val group = await(usersService.createStudentGroup(Data.group1))
    val group2 = await(usersService.createStudentGroup(Data.group2))

    val student1 = await(
      usersService.createUser(Data.student(group.id.get, "student1", "Іван Іванов", "1"))
    )
    val student2 = await(
      usersService.createUser(Data.student(group2.id.get, "student2", "Петро Петренко", "2"))
    )
    val student3 = await(
      usersService.createUser(Data.student(group2.id.get, "student3", "Максим Максименко", "3"))
    )
    val admin = await(usersService.createUser(Data.userAdmin))
    val instructor = await(usersService.createUser(Data.userInstructor))

    //b3FhdWpiamg1Y2F2c2c0ZXQ0MmVpbXVhOWh2cWUzaTlxNWhoYzVoaW9hNXV2YWd2dGg5bXUwM2htMCYxJjE0ODU1MzUxMDQwMDA
    val studentToken = insertToken(Data.userToken(student1.id.get))
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
