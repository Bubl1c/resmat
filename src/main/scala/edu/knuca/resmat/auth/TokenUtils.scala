package edu.knuca.resmat.auth

import java.util.{Base64, UUID}

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.GeneralHelpers
import org.joda.time.{DateTime, DateTimeZone}

import scala.concurrent.duration._
import scala.util.{Random, Try}

/**
  * Util class for generating private and public token parts.
  *
  * Moved from quad-laser's model shared library.
  */
object TokenUtils extends LazyLogging {

  private val EncryptionKey = "4eu519mhm6tf8se6tg65tso8es00to650"

  val expiryTime = 1 day

  private val oneToNine = '1' to '9'
  private def firstZeroReplacement = oneToNine(Random.nextInt(oneToNine.length)).toString

  def createToken(userId: Long, expiration: Option[Duration] = Some(expiryTime)): TokenEntity = {
    val token = UUID.randomUUID().toString.replaceAll("-","").replaceFirst("^0", firstZeroReplacement)
    val created = DateTime.now(DateTimeZone.UTC)
    val expires = expiration map (exp => created.plus(exp.toMillis))
    TokenEntity(None, userId, token, created, expires)
  }

  def encode(token: TokenEntity): EncodedToken = {
    encode(token.token, token.userId, token.created, token.expires)
  }

  private def encode(token: String, userId: Long, created: DateTime, expires: Option[DateTime]): EncodedToken = {
    val encrypted = s"${encrypt(token)}&$userId&${created.getMillis}"
    val encodedAccessToken = encode(encrypted)
    EncodedToken(encodedAccessToken, userId, created, expires)
  }

  /**
    * Encrypts token private part using private encryption key
    *
    * @param string token private part
    * @return encrypted token private part
    */
  def encrypt(string: String): String = (BigInt(string, 16) * BigInt(EncryptionKey, 32)).toString(32)

  /**
    * Decrypts token private part using private encryption key
    *
    * @param encrypted encrypted token private part
    * @return decrypted token private part
    */
  def decrypt(encrypted: String): String = (BigInt(encrypted, 32) / BigInt(EncryptionKey, 32)).toString(16)

  /***
    * Encodes entire concatenated token (private part and context information) using [[Base64]] algorithm
    *
    * @param tokenInformation entire token with private part and context information
    * @return encoded token used for authorization
    */
  def encode(tokenInformation: String): String = Base64.getEncoder.encodeToString(tokenInformation.getBytes("US-ASCII"))

  /***
    * Decodes token using [[Base64]] algorithm to token with private part and context information
    *
    * @param tokenInformation encoded token
    * @return decoded token from authorization
    */
  def decode(tokenInformation: String): String = new String(Base64.getDecoder.decode(tokenInformation))

  def parse(authHeader : String) : Option[DecodedToken] = {
    val parsedTokenTry = Try {
      val Array(token, userId, created) = TokenUtils.decode(authHeader).split("&")
      DecodedToken(decrypt(token), userId.toLong, GeneralHelpers.toUTC(created.toLong))
    }
    if(parsedTokenTry.isFailure) {
      logger.warn(s"Token parsing error. authHeader : $authHeader", parsedTokenTry.failed.get)
    }
    parsedTokenTry.toOption
  }
}
