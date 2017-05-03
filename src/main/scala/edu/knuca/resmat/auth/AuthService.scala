package edu.knuca.resmat.auth

import java.sql.Connection
import java.util.UUID

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.GeneralHelpers
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.http.UnauthenticatedException
import edu.knuca.resmat.user.{AuthenticatedUser, UserEntity, UserType, UsersService}
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class DefaultAuthService(val db: DatabaseService)
                        (val tokenGenerator: TokenGenerator, val usersService: UsersService)
                        (implicit val executionContext: ExecutionContext) extends AuthService with LazyLogging {}

trait AuthService { this: LazyLogging =>

  def db: DatabaseService
  def tokenGenerator: TokenGenerator
  def usersService: UsersService

  implicit val executionContext: ExecutionContext

  def authenticate(token: DecodedToken): Future[Option[AuthenticatedUser]] = Future {
    db.run{implicit c =>
      val tokenEntityOpt = TokensQueries.getByToken(token.token, token.created).as(TokensQueries.parser.singleOpt)
      val userEntityOpt = tokenEntityOpt match {
        case Some(tokenEntity) => Await.result(usersService.getById(tokenEntity.userId), 5 seconds)
        case None => None
      }
      userEntityOpt.map(u => AuthenticatedUser(u.id.get, u.username, u.email, u.userType, u.studentGroupId))
    }
  }

  def signIn(login: String, password: String): Future[EncodedToken] = Future {
    db.run{implicit c =>
      val token = usersService.getBy(login, password).flatMap{ userEntity =>
        Some(getOrCreateToken(userEntity.id.get))
      }
      TokenUtils.encode(token.getOrElse(
        throw UnauthenticatedException()
      ))
    }
  }

  def logout(authenticatedUser: AuthenticatedUser): Future[Unit] = Future {
    db.run{implicit c =>
      TokensQueries.deleteByUserId(authenticatedUser.id).executeUpdate()
    }
  }

  def signInWithAccessKey(accessKey: String): Future[EncodedToken] = Future {
    db.run{implicit c =>
      val token = usersService.getByAccessKey(accessKey).flatMap{ userEntity =>
        if(userEntity.userType != UserType.Student) {
          throw UnauthenticatedException()
        }
        Some(getOrCreateToken(userEntity.id.get))
      }
      TokenUtils.encode(token.getOrElse(
        throw UnauthenticatedException()
      ))
    }
  }

  def createToken(userId: Long): Future[TokenEntity] = Future {
    logger.debug(s"Creating token for user id: $userId")
    val tokenEntity = TokenUtils.createToken(userId)
    db.run { implicit c =>
      val tokenIdOpt: Option[Long] =
        TokensQueries.insert(userId, tokenEntity.token, tokenEntity.created, tokenEntity.expires).executeInsert()
      tokenIdOpt match {
        case Some(tokenId) => TokensQueries.getById(tokenId).as(TokensQueries.parser.single)
        case None => throw new RuntimeException("Failed to insert token for user id: " + userId)
      }
    }
  }

  private def getOrCreateToken(userId: Long)(implicit c: Connection): TokenEntity = {
    TokensQueries.getByUserId(userId).as(TokensQueries.parser.singleOpt) match {
      case Some(tokenEntity) => tokenEntity
      case None => Await.result(createToken(userId), 5 seconds)
    }
  }

}

trait TokenGenerator {
  def generate: String
}

class UUIDTokenGenerator extends TokenGenerator {
  override def generate = UUID.randomUUID().toString.replaceAll("-", "")
}

object TokensQueries {
  import anorm.SqlParser.{date, long, str}

  val parser = for {
    id <- long("id")
    userId <- long("user_id")
    token <- str("token")
    created <- date("created")
    expires <- date("expires").?
  } yield TokenEntity(Some(id), userId, token, new DateTime(created), expires.map(new DateTime(_)))

  def insert(userId: Long, token: String, created: DateTime, expires: Option[DateTime] = None) = SQL(
    """
      |INSERT INTO tokens (user_id, token, created, expires) VALUES ({userId}, {token}, {created}, {expires})
    """.stripMargin
  ).on(
    "userId" -> userId,
    "token" -> token,
    "created" -> GeneralHelpers.toMysql(created),
    "expires" -> expires.map(GeneralHelpers.toMysql)
  )

  val getAll = SQL("SELECT * FROM tokens")

  def getById(tokenId: Long) = SQL("SELECT * FROM tokens WHERE id = {tokenId}").on("tokenId" -> tokenId)

  def getByUserId(userId: Long) = SQL("SELECT * FROM tokens WHERE user_id = {userId}").on("userId" -> userId)

  def getByToken(token: String, created: DateTime) =
    SQL("SELECT * FROM tokens WHERE token = {token} AND created = {created} AND expires > now()")
      .on("token" -> token, "created" -> GeneralHelpers.toMysql(created))

  def deleteByUserId(userId: Long) = SQL("DELETE FROM tokens WHERE user_id = {userId}").on("userId" -> userId)
}
