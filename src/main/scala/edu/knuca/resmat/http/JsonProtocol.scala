package edu.knuca.resmat.http

import edu.knuca.resmat.user.UserType
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scala.util.Try

object JsonProtocol {

  implicit val encodeDateTime: Encoder[DateTime] = new Encoder[DateTime] {
    final def apply(a: DateTime): Json = ISODateTimeFormat.dateTime().print(a).asJson
  }
  implicit val decodeDateTime: Decoder[DateTime] = Decoder.decodeString.emap { str =>
    Try(ISODateTimeFormat.dateTime().parseDateTime(str)) match {
      case scala.util.Success(dateTime) => Right(dateTime)
      case scala.util.Failure(t) => Left(t.getMessage)
    }
  }

  implicit val encodeUserType: Encoder[UserType.UserType] = Encoder.encodeString.contramap[UserType.UserType](_.toString)
  implicit val decodeUserType: Decoder[UserType.UserType] = Decoder.decodeString.emap { str =>
    Try(UserType.withName(str)) match {
      case scala.util.Success(ut) => Right(ut)
      case scala.util.Failure(t) => Left(t.getMessage)
    }
  }
}
