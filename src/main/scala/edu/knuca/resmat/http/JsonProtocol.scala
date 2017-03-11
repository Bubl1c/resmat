package edu.knuca.resmat.http

import edu.knuca.resmat.exam._
import edu.knuca.resmat.user.UserType
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scala.util.{Failure, Success, Try}

object JsonProtocol {

  implicit val encodeDateTime: Encoder[DateTime] = new Encoder[DateTime] {
    final def apply(a: DateTime): Json = ISODateTimeFormat.dateTime().print(a).asJson
  }
  implicit val decodeDateTime: Decoder[DateTime] = Decoder.decodeString.emap { str =>
    Try(ISODateTimeFormat.dateTime().parseDateTime(str)) match {
      case Success(dateTime) => Right(dateTime)
      case Failure(t) => Left(t.getMessage)
    }
  }

  implicit val encodeUserType: Encoder[UserType.UserType] = Encoder.encodeString.contramap[UserType.UserType](_.toString)
  implicit val decodeUserType: Decoder[UserType.UserType] = enumDecoder(UserType)

  implicit val encodeESDT: Encoder[ExamStepType.ExamStepType] = Encoder.encodeString.contramap[ExamStepType.ExamStepType](_.toString)
  implicit val decodeESDT: Decoder[ExamStepType.ExamStepType] = enumDecoder(ExamStepType)

  implicit val encodeTaskType: Encoder[TaskType.TaskType] = Encoder.encodeString.contramap[TaskType.TaskType](_.toString)
  implicit val decodeTaskType: Decoder[TaskType.TaskType] = enumDecoder(TaskType)

  implicit val encodeTestType: Encoder[TestType.TestType] = Encoder.encodeString.contramap[TestType.TestType](_.toString)
  implicit val decodeTestType: Decoder[TestType.TestType] = enumDecoder(TestType)

  implicit val encodeExamStatus: Encoder[ExamStatus.ExamStatus] = Encoder.encodeString.contramap[ExamStatus.ExamStatus](_.toString)
  implicit val decodeExamStatus: Decoder[ExamStatus.ExamStatus] = enumDecoder(ExamStatus)

  implicit val encodeExamStepStatus: Encoder[ExamStepStatus.ExamStepStatus] = Encoder.encodeString.contramap[ExamStepStatus.ExamStepStatus](_.toString)
  implicit val decodeExamStepStatus: Decoder[ExamStepStatus.ExamStepStatus] = enumDecoder(ExamStepStatus)

  implicit val encodeTOVT: Encoder[TestOptionValueType.TestOptionValueType] = Encoder.encodeString.contramap[TestOptionValueType.TestOptionValueType](_.toString)
  implicit val decodeTOVT: Decoder[TestOptionValueType.TestOptionValueType] = enumDecoder(TestOptionValueType)

  def enumDecoder(en: Enumeration) = Decoder.decodeString.emap { str =>
    Try(en.withName(str)) match {
      case scala.util.Success(ut) => Right(ut)
      case scala.util.Failure(t) => Left(t.getMessage)
    }
  }

  implicit val encodeStepData: Encoder[StepDataDto] = new Encoder[StepDataDto] {
    import io.circe.generic.auto._
    override def apply(a: StepDataDto) = a match {
      case tsd: TestSetDto => tsd.asJson
      case ni: NI => ni.asJson
    }
  }
}
