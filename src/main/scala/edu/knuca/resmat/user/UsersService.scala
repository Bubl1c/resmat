package edu.knuca.resmat.user

import java.sql.Connection

import anorm.{BatchSql, NamedParameter, SQL}
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

  def createStudents(groupId: Long, users: Seq[UserEntity], replaceExisting: Option[Boolean]): Future[Unit] = Future {
    db.runTransaction{ implicit c =>
      if (replaceExisting.getOrElse(false)) {
        val existingUsers = UsersQueries.getStudentsByGroup(groupId).as(UsersQueries.parser.*)
        existingUsers.foreach(u => {
          deleteUserInternal(u.id.get)
        })
      }
      users.foreach(user => {
        logger.debug(s"Creating user: $user")
        val insertedUserIdOpt: Option[Long] = UsersQueries.insert(user).executeInsert()
        if (insertedUserIdOpt.isEmpty) {
          throw new RuntimeException(s"User wasn't created, failed to insert. $user")
        }
      })
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
  
  private def deleteUserInternal(id: Long)(implicit c: Connection): Int = {
    UsersQueries.delete(id).executeUpdate()
  }
  
  def deleteUser(id: Long, checkType: Option[UserType.UserType] = None): Future[Int] = Future { db.run { implicit c =>
    val userOpt = UsersQueries.getById(id).as(UsersQueries.parser.singleOpt)
    userOpt match {
      case Some(user) =>
        checkUserType(user, checkType).get
        deleteUserInternal(id)
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

  def createStudentGroup(group: StudentGroupEntity, ownerUserIdOpt: Option[Long] = None): Future[StudentGroupEntity] = Future {
    db.runTransaction { implicit c =>
      logger.debug(s"Creating group: $group")
      val groupIdOpt: Option[Long] = UsersQueries.insertStudentGroup(group).executeInsert()
      groupIdOpt match {
        case Some(groupId) => {
          ownerUserIdOpt.foreach(ownerUserId => {
            UsersQueries.grantAccessToStudentGroups(ownerUserId, Set(groupId)).execute()
          })
          group.copy(id = Some(groupId))
        }
        case None => throw new RuntimeException(s"User group wasn't created, failed to insert. $group")
      }
    }
  }

  def getAccessToStudentGroups(userId: Long): UserStudentGroupAccessDto = db.run{ implicit c =>
    val sgIds = UsersQueries.getAccessToStudentGroups(userId).as(UsersQueries.userStudentGroupAccessParser.*)
    UserStudentGroupAccessDto(userId, sgIds.toSet)
  }

  def setUserStudentGroupAccess(dto: UserStudentGroupAccessDto): Unit = db.runTransaction { implicit c =>
    UsersQueries.removeAccessToStudentGroups(dto.userId).execute()
    if (dto.studentGroupIds.nonEmpty) {
      UsersQueries.grantAccessToStudentGroups(dto.userId, dto.studentGroupIds).execute()
    }
  }

  def getAllStudentGroups(isArchived: Option[Boolean] = None, onlyAccessible: Boolean = false)(implicit user: AuthenticatedUser): Future[Seq[StudentGroupEntity]] = Future {
    val accessibleForUserId = if (onlyAccessible) { Some(user.id) } else None
    db.run { implicit c =>
      UsersQueries.getAllStudentGroups(isArchived, accessibleForUserId).as(UsersQueries.groupParser.*)
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
  import anorm.SqlParser.{int, long, str, bool}
  
  object U {
    val table = "users"
    val id = "id"
    val groupId = "group_id"
  }

  object SG {
    val table = "student_groups"
    val id = "id"
    val name = "name"
    val isArchived = "is_archived"
  }

  object SGA {
    val table = "student_group_articles"
    val studentGroupId = "student_group_id"
    val articleId = "article_id"
  }

  object UsSGs {
    val table = "users_student_groups"
    val userId = "user_id"
    val studentGroupId = "student_group_id"
  }
  val userStudentGroupAccessParser = for {
    studentGroupId <- long(UsSGs.studentGroupId)
  } yield studentGroupId

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
    isArchived <- bool(SG.isArchived)
  } yield StudentGroupEntity(Some(id), name, isArchived)

  def insertStudentGroup(group: StudentGroupEntity) = SQL(
    s"""
      |INSERT INTO ${SG.table} (${SG.name})
      |VALUES ({name})
    """.stripMargin
  ).on("name" -> group.name)

  def getAccessToStudentGroups(userId: Long) = {
    SQL(s"SELECT * FROM ${UsSGs.table} WHERE ${UsSGs.userId} = {userId}")
      .on("userId" -> userId)
  }

  def grantAccessToStudentGroups(userId: Long, sgIds: Set[Long]): BatchSql = {
    val values = sgIds.toSeq.map( ecId =>
      Seq[NamedParameter](
        "userId" -> userId,
        "studentGroupId" -> ecId
      )
    )
    BatchSql(
      s"""INSERT INTO ${UsSGs.table} (
         |${UsSGs.userId},
         |${UsSGs.studentGroupId}
         |) VALUES (
         |{userId},
         |{studentGroupId}
         |)""".stripMargin,
      values.head,
      values.tail : _*
    )
  }

  def removeAccessToStudentGroups(userId: Long) = {
    SQL(s"DELETE FROM ${UsSGs.table} WHERE ${UsSGs.userId} = {userId}")
      .on("userId" -> userId)
  }

  def getAllStudentGroups(isArchived: Option[Boolean], accessibleForUserId: Option[Long]) = {
    val archivedCondition = isArchived
      .map(ia => s"WHERE ${SG.isArchived} IS $ia")
      .getOrElse(s"WHERE ${SG.isArchived} IS FALSE")
    val accessibleJoin = accessibleForUserId
      .map(userId => s"JOIN ${UsSGs.table} ON ${UsSGs.table}.${UsSGs.studentGroupId} = ${SG.table}.${SG.id} AND ${UsSGs.table}.${UsSGs.userId} = $userId")
      .getOrElse("")
    SQL(
      s"SELECT ${SG.table}.* FROM ${SG.table} $accessibleJoin $archivedCondition"
    )
  }

  def getStudentGroupById(groupId: Long) = SQL(s"SELECT * FROM ${SG.table} WHERE ${SG.id} = {groupId}").on("groupId" -> groupId)

  def updateStudentGroup(groupId: Long, groupUpdate: StudentGroupEntityUpdate) = {
    val query = SQL(s"UPDATE ${SG.table} SET ${SG.name} = {name}, ${SG.isArchived} = {isArchived} WHERE ${SG.id} = {id}")
      .on("id" -> groupId, "name" -> groupUpdate.name, "isArchived" -> groupUpdate.isArchived)
    query
  }

  def createGroupArticles(groupId: Long, articleIds: Seq[Long]) =
    SQL(s"INSERT INTO ${SGA.table} (${SGA.studentGroupId}, ${SGA.articleId}) VALUES ${articleIds.map(aid => s"($groupId, $aid)").mkString(", ")}")

  def deleteGroupArticles(groupId: Long) =
    SQL(s"DELETE FROM ${SGA.table} WHERE ${SGA.studentGroupId} = {groupId}").on("groupId" -> groupId)
}