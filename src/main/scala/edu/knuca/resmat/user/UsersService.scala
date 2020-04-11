package edu.knuca.resmat.user

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class DefaultUsersService(val db: DatabaseService)
                         (implicit val executionContext: ExecutionContext) extends UsersService with LazyLogging {
}

trait UsersService { this: LazyLogging =>

  def db: DatabaseService
  implicit val executionContext: ExecutionContext

  def getByAccessKey(accessKey: String): Option[UserEntity] = {
    db.run { implicit c =>
      UsersQueries.getByAccessKey(accessKey).as(UsersQueries.parser.singleOpt)
    }
  }

  def getAllNotStudents(): Future[Seq[UserEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getAll.as(UsersQueries.parser.*).filter(_.userType != UserType.Student)
    }
  }

  def getById(id: Long): Future[Option[UserEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getById(id).as(UsersQueries.parser.singleOpt)
    }
  }

  private def getByIdWithPassword(id: Long): Future[Option[UserEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getById(id).as(UsersQueries.parserWithPassword.singleOpt)
    }
  }

  def getByIdInStudentGroup(id: Long, groupId: Long): Future[Option[UserEntity]] =
    getStudentsByGroup(groupId).flatMap(students =>
      Future.successful(students.find(_.id.contains(id)))
    )

  def getBy(username: String, password: String): Option[UserEntity] = {
    db.run { implicit c =>
      UsersQueries.getBy(username, password).as(UsersQueries.parser.singleOpt)
    }
  }

  def getBy(userType: UserType.UserType): Future[Seq[UserEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getBy(userType).as(UsersQueries.parser.*)
    }
  }

  def getStudentsByGroup(userGroupId: Long): Future[Seq[UserEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getStudentsByGroup(userGroupId).as(UsersQueries.parser.*)
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

  def updateUser(id: Long, userUpdate: UserEntityUpdate, requireUserType: Option[UserType.UserType] = None): Future[Option[UserEntity]] = getByIdWithPassword(id).flatMap {
    case Some(userEntity) =>
      if(requireUserType.isDefined && userEntity.userType != requireUserType.get) {
        throw new IllegalArgumentException(
          s"Cannot update user with type ${userEntity.userType.toString}, required user type ${requireUserType.get.toString}"
        )
      }
      logger.debug(s"Updating user. id: $id, update: $userUpdate")
      db.runTransaction{implicit c =>
        val updatedUser = userUpdate.merge(userEntity)
        val rowsUpdated = UsersQueries.update(updatedUser).executeUpdate()
        if (rowsUpdated != 1) throw new RuntimeException("Failed to update user, rows updated: " + rowsUpdated)
        Future.successful(Some(updatedUser))
      }
    case None => Future.successful(None)
  }

  def deleteUser(id: Long, checkType: Option[UserType.UserType] = None): Future[Int] = Future { db.run { implicit c =>
    val userOpt = UsersQueries.getById(id).as(UsersQueries.parser.singleOpt)
    userOpt match {
      case Some(user) =>
        checkUserType(user, checkType).get
        UsersQueries.delete(id).executeUpdate()
      case None => 0
    }
  }}

  def moveStudentToGroup(studentId: Long, groupId: Long): Future[Option[UserEntity]] = getById(studentId).flatMap {
    case Some(userEntity) =>
      checkUserType(userEntity, Some(UserType.Student)).get
      db.runTransaction{implicit c =>
        val rowsUpdated = UsersQueries.moveStudentToGroup(userEntity.id.get, groupId).executeUpdate()
        if (rowsUpdated != 1)
          throw new RuntimeException("Failed to move student to another group, rows updated: " + rowsUpdated)
        Future.successful(
          Some(userEntity.copy(studentGroupId = Some(groupId)))
        )
      }
    case None => Future.successful(None)
  }

  /*===================== GROUPS ============================*/

  def createStudentGroup(group: StudentGroupEntity): Future[StudentGroupEntity] = Future {
    db.run{ implicit c =>
      logger.debug(s"Creating group: $group")
      val groupIdOpt: Option[Long] = UsersQueries.insertStudentGroup(group).executeInsert()
      groupIdOpt match {
        case Some(groupId) => group.copy(id = Some(groupId))
        case None => throw new RuntimeException(s"User group wasn't created, failed to insert. $group")
      }
    }
  }

  def getAllStudentGroups(): Future[Seq[StudentGroupEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getAllStudentGroups().as(UsersQueries.groupParser.*)
    }
  }

  def getStudentGroupById(groupId: Long): Future[Option[StudentGroupEntity]] = Future {
    db.run { implicit c =>
      UsersQueries.getStudentGroupById(groupId).as(UsersQueries.groupParser.singleOpt)
    }
  }

  def updateStudentGroup(groupId: Long, groupUpdate: StudentGroupEntityUpdate): Future[StudentGroupEntity] = {
    db.runTransaction { implicit c =>
      val rowsUpdated = UsersQueries.updateStudentGroup(groupId, groupUpdate).executeUpdate()
      if(rowsUpdated != 1) {
        throw new RuntimeException(s"Failed to update user group with id: $groupId. Updated rows: $rowsUpdated")
      }
      getStudentGroupById(groupId).map(_.getOrElse(
        throw new RuntimeException(s"Failed to update user group with id: $groupId. Failed to fetch by id")
      ))
    }
  }

  def setArticlesToGroup(groupId: Long, articleIds: Seq[Long]): Unit = {
    db.runTransaction { implicit c =>
      UsersQueries.deleteGroupArticles(groupId).executeUpdate()
      if(articleIds.nonEmpty) {
        UsersQueries.createGroupArticles(groupId, articleIds).executeInsert()
      }
    }
  }

  private def checkUserType(user: UserEntity, checkTypeOpt: Option[UserType.UserType]): Try[UserEntity] = Try {
    checkTypeOpt.foreach( checkType =>
      require(
        user.userType == checkType,
        s"Type check failed, required: $checkType, found: ${user.userType}"
      )
    )
    user
  }

}

object UsersQueries {
  import anorm.SqlParser.{int, long, str}
  
  object U {
    val table = "users"
    val id = "id"
    val groupId = "group_id"
  }

  object SGA {
    val table = "student_group_articles"
    val studentGroupId = "student_group_id"
    val articleId = "article_id"
  }

  val parserWithPassword  = for {
    id <- long("id")
    username <- str("username")
    password <- str("password")
    firstName <- str("first_name")
    lastName <- str("last_name")
    email <- str("email")
    userType <- int("user_type")
    groupId <- long("group_id").?
    accessKey <- str("access_key")
  } yield UserEntity(Some(id), username, password, firstName, lastName, email, UserType(userType), accessKey, groupId)

  val parser = parserWithPassword.map(_.copy(password = ""))

  def insert(user: UserEntity) = SQL(
    """
      |INSERT INTO users (username, password, first_name, last_name, email, user_type, access_key, group_id)
      |VALUES ({username}, {password}, {firstName}, {lastName}, {email}, {userType}, {accessKey}, {groupId})
    """.stripMargin
  ).on(
    "username" -> user.username,
    "password" -> user.password,
    "firstName" -> user.firstName,
    "lastName" -> user.lastName,
    "email" -> user.email,
    "userType" -> user.userType.id,
    "accessKey" -> user.accessKey,
    "groupId" -> user.studentGroupId)

  def update(user: UserEntity) =
    SQL(
      """UPDATE users
        |SET username={username}, password={password}, first_name={firstName}, last_name={lastName}, email={email},
        | access_key={accessKey}
        |WHERE id = {userId}""".stripMargin)
      .on(
        "userId" -> user.id.get,
        "username" -> user.username,
        "password" -> user.password,
        "firstName" -> user.firstName,
        "lastName" -> user.lastName,
        "email" -> user.email,
        "accessKey" -> user.accessKey)

  def moveStudentToGroup(userId: Long, groupId: Long) =
    SQL("UPDATE users SET group_id={groupId} WHERE id = {userId}")
    .on("userId" -> userId, "groupId" -> groupId)

  def delete(userId: Long) = SQL("DELETE FROM users WHERE id = {userId}").on("userId" -> userId)

  val getAll = SQL("SELECT * FROM users")

  def getById(userId: Long) = SQL("SELECT * FROM users WHERE id = {userId}").on("userId" -> userId)

  def getBy(username: String, password: String) = SQL(
    "SELECT * FROM users WHERE username = {username} AND password = {password}"
  ).on("username" -> username, "password" -> password)

  def getBy(userType: UserType.UserType) = SQL(
    "SELECT * FROM users WHERE user_type = {userType}"
  ).on("userType" -> userType.id)

  def getStudentsByGroup(groupId: Long) = SQL(
    "SELECT * FROM users WHERE user_type = {userType} AND group_id = {groupId}"
  ).on("userType" -> UserType.Student.id, "groupId" -> groupId)

  def getByAccessKey(accessKey: String) = SQL(
    "SELECT * FROM users WHERE access_key = {accessKey}"
  ).on("accessKey" -> accessKey)

  /*===================== GROUPS ============================*/

  val groupParser = for {
    id <- long("id")
    name <- str("name")
  } yield StudentGroupEntity(Some(id), name)

  def insertStudentGroup(group: StudentGroupEntity) = SQL(
    """
      |INSERT INTO student_groups (name)
      |VALUES ({name})
    """.stripMargin
  ).on("name" -> group.name)

  def getAllStudentGroups() = SQL("SELECT * FROM student_groups")

  def getStudentGroupById(groupId: Long) = SQL("SELECT * FROM student_groups WHERE id = {groupId}").on("groupId" -> groupId)

  def updateStudentGroup(groupId: Long, groupUpdate: StudentGroupEntityUpdate) =
    SQL("UPDATE student_groups SET name = {name} WHERE id = {id}").on("id" -> groupId, "name" -> groupUpdate.name)

  def createGroupArticles(groupId: Long, articleIds: Seq[Long]) =
    SQL(s"INSERT INTO ${SGA.table} (${SGA.studentGroupId}, ${SGA.articleId}) VALUES ${articleIds.map(aid => s"($groupId, $aid)").mkString(", ")}")

  def deleteGroupArticles(groupId: Long) =
    SQL(s"DELETE FROM ${SGA.table} WHERE ${SGA.studentGroupId} = {groupId}").on("groupId" -> groupId)
}