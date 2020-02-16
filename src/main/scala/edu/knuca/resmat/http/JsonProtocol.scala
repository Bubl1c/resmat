package edu.knuca.resmat.http

import edu.knuca.resmat.core.crosssection.{ShapeRotationAngle, ShapeType}
import edu.knuca.resmat.core.ringplate.BindingType
import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.taskflow.TaskFlowDto
import edu.knuca.resmat.exam.testset.TestSetDto
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

  implicit val encodeTaskType: Encoder[ProblemType.ProblemType] = Encoder.encodeString.contramap[ProblemType.ProblemType](_.toString)
  implicit val decodeTaskType: Decoder[ProblemType.ProblemType] = enumDecoder(ProblemType)

  implicit val encodeTestType: Encoder[TestType.TestType] = Encoder.encodeString.contramap[TestType.TestType](_.toString)
  implicit val decodeTestType: Decoder[TestType.TestType] = enumDecoder(TestType)

  implicit val encodeExamStatus: Encoder[ExamStatus.ExamStatus] = Encoder.encodeString.contramap[ExamStatus.ExamStatus](_.toString)
  implicit val decodeExamStatus: Decoder[ExamStatus.ExamStatus] = enumDecoder(ExamStatus)

  implicit val encodeExamStepStatus: Encoder[ExamStepStatus.ExamStepStatus] = Encoder.encodeString.contramap[ExamStepStatus.ExamStepStatus](_.toString)
  implicit val decodeExamStepStatus: Decoder[ExamStepStatus.ExamStepStatus] = enumDecoder(ExamStepStatus)

  implicit val encodeTOVT: Encoder[TestOptionValueType.TestOptionValueType] = Encoder.encodeString.contramap[TestOptionValueType.TestOptionValueType](_.toString)
  implicit val decodeTOVT: Decoder[TestOptionValueType.TestOptionValueType] = enumDecoder(TestOptionValueType)

  implicit val encodeTFST: Encoder[TaskFlowStepType.TaskFlowStepType] = Encoder.encodeString.contramap[TaskFlowStepType.TaskFlowStepType](_.toString)
  implicit val decodeTFST: Decoder[TaskFlowStepType.TaskFlowStepType] = enumDecoder(TaskFlowStepType)

  implicit val encodeBindingType: Encoder[BindingType.BindingType] = Encoder.encodeString.contramap[BindingType.BindingType](_.toString)
  implicit val decodeBindingType: Decoder[BindingType.BindingType] = enumDecoder(BindingType)

  implicit val encodeShapeType: Encoder[ShapeType.ShapeType] = Encoder.encodeString.contramap[ShapeType.ShapeType](_.toString)
  implicit val decodeShapeType: Decoder[ShapeType.ShapeType] = enumDecoder(ShapeType)

  implicit val encodeShapeRotationAngle: Encoder[ShapeRotationAngle.ShapeRotationAngle] = Encoder.encodeString.contramap[ShapeRotationAngle.ShapeRotationAngle](_.toString)
  implicit val decodeShapeRotationAngle: Decoder[ShapeRotationAngle.ShapeRotationAngle] = enumDecoder(ShapeRotationAngle)

  implicit val encodeProblemVariantSchemaType: Encoder[ResmatImageType.ResmatImageType] = Encoder.encodeString.contramap[ResmatImageType.ResmatImageType](_.toString)
  implicit val decodeProblemVariantSchemaType: Decoder[ResmatImageType.ResmatImageType] = enumDecoder(ResmatImageType)

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
      case tfd: TaskFlowDto => tfd.asJson
      case uer: UserExamResult => uer.asJson
      case ni: NI => ni.asJson
    }
  }
}
