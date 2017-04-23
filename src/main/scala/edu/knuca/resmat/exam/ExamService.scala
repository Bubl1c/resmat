package edu.knuca.resmat.exam

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.concurrent.ExecutionContext
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

class ExamService(val db: DatabaseService)(implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.exam.{ExamQueries => Q}

  def createExamConf(ec: ExamConf): ExamConf = db.run{ implicit c =>
    val insertedIdOpt: Option[Long] = Q.createExamConf(ec).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert exam conf: $ec")
    )
    getExamConf(insertedId)
  }

  def createExamStepConf(esc: ExamStepConf): ExamStepConf = db.run{ implicit c =>
    val insertedIdOpt: Option[Long] = Q.createExamStepConf(esc).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert exam step conf: $esc")
    )
    getExamStepConf(insertedId)
  }

  def getExamConf(id: Long): ExamConf = db.run{ implicit c =>
    Q.getExamConf(id).as(Q.examConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Cannot find exam conf by id: $id")
    )
  }

  def getExamStepConf(id: Long): ExamStepConf = db.run { implicit c =>
    Q.getExamStepConf(id).as(Q.examStepConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Exam step conf with id: $id not found.")
    )
  }

  def findExamStepConfsByExamConfId(examConfId: Long): Seq[ExamStepConf] = db.run{ implicit c =>
    Q.findExamStepConfsByExamConfId(examConfId).as(Q.examStepConfParser.*)
  }

  def findSubmittableExamStepConf(examConfId: Long): Seq[ExamStepConf] = db.run{ implicit c =>
    Q.findSubmittableExamStepConf(examConfId).as(Q.examStepConfParser.*)
  }

  def getExamStepConfByExamConfIdAndSequence(examConfId: Long, stepSequence: Int): ExamStepConf = db.run{ implicit c =>
    Q.getExamStepConfByExamConfIdAndSequence(examConfId, stepSequence).as(Q.examStepConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Exam step conf for exam conf id: $examConfId, with sequence: $stepSequence not found.")
    )
  }

}

object ExamQueries {
  import anorm.SqlParser.{int, long, str, bool}
  
  object E {
    val table = "exam_confs"
    val id = "id"
    val name = "name"
    val description = "description"
    val maxScore = "max_score"
  }
  
  object ES {
    val table = "exam_step_confs"
    val id = "id"
    val examConfId = "exam_conf_id"
    val sequence = "sequence"
    val name = "name"
    val stepType = "step_type"
    val mistakesPerAttemptLimit = "mistakes_per_attempt_limit"
    val mistakeValuePercents = "mistake_value_percents"
    val attemptsLimit = "attempts_limit"
    val attemptValuePercents = "attempt_value_percents"
    val maxScore = "max_score"
    val hasToBeSubmitted = "has_to_be_submitted"
    val dataSet = "data_set"
  }

  val examConfParser  = for {
    id <- long(E.id)
    name <- str(E.name)
    description <- str(E.description)
    maxScore <- int(E.maxScore)
  } yield ExamConf(id, name, description, maxScore)

  val examStepConfParser = for {
    id <- long(ES.id)
    examConfId <- long(ES.examConfId)
    sequence <- int(ES.sequence)
    name <- str(ES.name)
    stepType <- int(ES.stepType)
    mistakesPerAttemptLimit <- int(ES.mistakesPerAttemptLimit)
    mistakeValue <- int(ES.mistakeValuePercents)
    attemptsLimit <- int(ES.attemptsLimit)
    attemptValue <- int(ES.attemptValuePercents)
    maxScore <- int(ES.maxScore)
    dataSet <- str(ES.dataSet)
    hasToBeSubmitted <- bool(ES.hasToBeSubmitted)
  } yield ExamStepConf(
    id,
    examConfId,
    sequence,
    name,
    ExamStepType(stepType),
    mistakesPerAttemptLimit,
    mistakeValue,
    attemptsLimit,
    attemptValue,
    maxScore,
    decodeDataSet(dataSet),
    hasToBeSubmitted
  )

  def decodeDataSet(examStepConfDataSet: String): ExamStepConfDataSet = {
    decode[ExamStepConfDataSet](examStepConfDataSet).fold( e =>
      throw new RuntimeException(s"Failed to decode ExamStepConfDataSet: $examStepConfDataSet", e),
      r => r
    )
  }

  def createExamConf(ec: ExamConf) =
    SQL(s"INSERT INTO ${E.table} (${E.name}, ${E.description}, ${E.maxScore}) VALUES ({name}, {description}, {maxScore})")
      .on("name" -> ec.name)
      .on("description" -> ec.description)
      .on("maxScore" -> ec.maxScore)

  def createExamStepConf(esc: ExamStepConf) =
    SQL(
      s"""INSERT INTO ${ES.table} (
         |${ES.examConfId},
         |${ES.sequence},
         |${ES.name},
         |${ES.stepType},
         |${ES.mistakesPerAttemptLimit},
         |${ES.mistakeValuePercents},
         |${ES.attemptsLimit},
         |${ES.attemptValuePercents},
         |${ES.maxScore},
         |${ES.dataSet},
         |${ES.hasToBeSubmitted}
         |)
         |VALUES (
         |{examConfId},
         |{sequence},
         |{name},
         |{stepType},
         |{mistakesPerAttemptLimit},
         |{mistakeValuePercents},
         |{attemptsLimit},
         |{attemptValuePercents},
         |{maxScore},
         |{dataSet},
         |{hasToBeSubmitted}
         |)
       """.stripMargin)
      .on("examConfId" -> esc.examConfId)
      .on("sequence" -> esc.sequence)
      .on("name" -> esc.name)
      .on("stepType" -> esc.stepType.id)
      .on("mistakesPerAttemptLimit" -> esc.mistakesPerAttemptLimit)
      .on("mistakeValuePercents" -> esc.mistakeValuePercents)
      .on("attemptsLimit" -> esc.attemptsLimit)
      .on("attemptValuePercents" -> esc.attemptValuePercents)
      .on("maxScore" -> esc.maxScore)
      .on("dataSet" -> esc.dataSet.asJson.noSpaces)
      .on("hasToBeSubmitted" -> esc.hasToBeSubmitted)

  def getExamConf(id: Long) = SQL(s"SELECT * FROM ${E.table} WHERE ${E.id} = {id}").on("id" -> id)

  def getExamStepConf(id: Long) = SQL(s"SELECT * FROM ${ES.table} WHERE ${ES.id} = {id}").on("id" -> id)

  def findExamStepConfsByExamConfId(examConfId: Long) =
    SQL(s"SELECT * FROM ${ES.table} WHERE ${ES.examConfId} = {examConfId}")
      .on("examConfId" -> examConfId)

  def findSubmittableExamStepConf(examConfId: Long) =
    SQL(s"SELECT * FROM ${ES.table} WHERE ${ES.examConfId} = {examConfId} AND ${ES.hasToBeSubmitted} IS TRUE")
      .on("examConfId" -> examConfId)

  def getExamStepConfByExamConfIdAndSequence(examConfId: Long, stepSequence: Int) =
    SQL(s"SELECT * FROM ${ES.table} WHERE ${ES.examConfId} = {examConfId} AND ${ES.sequence} = {sequence}")
      .on("examConfId" -> examConfId)
      .on("sequence" -> stepSequence)
  
}
