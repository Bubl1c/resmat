package edu.knuca.resmat.user

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.concurrent.{ExecutionContext, Future}

class DefaultUsersService(val db: DatabaseService)
                         (implicit val executionContext: ExecutionContext) extends UsersService with LazyLogging {
}

trait UsersService { this: LazyLogging =>

  def db: DatabaseService
  implicit val executionContext: ExecutionContext

  def getBy(username: String, password: String): Option[UserEntity] = {
    db.run { implicit c =>
      UsersQueries.getBy(username, password).as(UsersQueries.parser.singleOpt)
    }
  }

  def getAll(): Future[Seq[UserEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getAll.as(UsersQueries.parser.*)
    }
  }

  def getUserById(id: Long): Future[Option[UserEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getById(id).as(UsersQueries.parser.singleOpt)
    }
  }

  def createGroup(group: UserGroupEntity): Future[UserGroupEntity] = Future {
    db.runTransaction{ implicit c =>
      logger.debug(s"Creating group: $group")
      val groupIdOpt: Option[Long] = UsersQueries.insert(group).executeInsert()
      groupIdOpt match {
        case Some(groupId) => group.copy(id = Some(groupId))
        case None => throw new RuntimeException(s"User group wasn't created, failed to insert. $group")
      }
    }
  }

  def createUser(user: UserEntity): Future[UserEntity] = Future {
    logger.debug(s"Creating user: $user")
    db.runTransaction{ implicit c =>
      val insertedUserIdOpt: Option[Long] = UsersQueries.insert(user).executeInsert()
      insertedUserIdOpt match {
        case Some(insertedUserId) => UsersQueries.getById(insertedUserId).as(UsersQueries.parser.singleOpt).getOrElse(
          throw new RuntimeException(s"User wasn't created, failed to fetch after insert. $user")
        )
        case None => throw new RuntimeException(s"User wasn't created, failed to insert. $user")
      }
    }
  }

  def updateUser(id: Long, userUpdate: UserEntityUpdate): Future[Option[UserEntity]] = db.runTransaction{implicit c =>
    logger.debug(s"Updating user. id: $id, update: $userUpdate")
    getUserById(id).flatMap {
      case Some(userEntity) =>
        val updatedUser = userUpdate.merge(userEntity)
        val rowsUpdated = UsersQueries.update(updatedUser).executeUpdate()
        if (rowsUpdated != 1) throw new RuntimeException("Failed to update user, rows updated: " + rowsUpdated)
        Future.successful(Some(updatedUser))
      case None => Future.successful(None)
    }
  }

  def deleteUser(id: Long): Future[Int] = Future { db.run { implicit c =>
    UsersQueries.delete(id).executeUpdate()
  }}
}

object UsersQueries {
  import anorm.SqlParser.{int, long, str}

  val parser = for {
    id <- long("id")
    username <- str("username")
    password <- str("password")
    firstName <- str("first_name")
    lastName <- str("last_name")
    email <- str("email")
    userType <- int("user_type")
    groupId <- long("group_id").?
  } yield UserEntity(Some(id), username, password, firstName, lastName, email, UserType(userType), groupId)

  val groupParser = for {
    id <- long("id")
    name <- str("name")
  } yield UserGroupEntity(Some(id), name)

  def insert(user: UserEntity) = SQL(
    """
      |INSERT INTO users (username, password, first_name, last_name, email, user_type, group_id)
      |VALUES ({username}, {password}, {firstName}, {lastName}, {email}, {userType}, {groupId})
    """.stripMargin
  ).on(
    "username" -> user.username,
    "password" -> user.password,
    "firstName" -> user.firstName,
    "lastName" -> user.lastName,
    "email" -> user.email,
    "userType" -> user.userType.id,
    "groupId" -> user.userGroupId)

  def insert(group: UserGroupEntity) = SQL(
    """
      |INSERT INTO user_groups (name)
      |VALUES ({name})
    """.stripMargin
  ).on("name" -> group.name)

  def update(user: UserEntity) =
    SQL("UPDATE users SET username={username}, password={password} WHERE id = {userId}")
      .on("username" -> user.username, "password" -> user.password)

  def delete(userId: Long) = SQL("DELETE FROM users WHERE id = {userId}").on("userId" -> userId)

  val getAll = SQL("SELECT * FROM users")

  def getById(userId: Long) = SQL("SELECT * FROM users WHERE id = {userId}").on("userId" -> userId)

  def getBy(username: String, password: String) = SQL(
    "SELECT * FROM users WHERE username = {username} AND password = {password}"
  ).on("username" -> username, "password" -> password)
}