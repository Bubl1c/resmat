package edu.knuca.resmat.exam

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.concurrent.ExecutionContext

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

  def createExamStepVariantConf(esvc: ExamStepVariantConf): ExamStepVariantConf = db.run{ implicit c =>
    val insertedIdOpt: Option[Long] = Q.createExamStepVariantConf(esvc).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert exam step variant conf: $esvc")
    )
    getExamStepVariantConf(insertedId)
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

  def getExamStepVariantConf(id: Long): ExamStepVariantConf = db.run { implicit c =>
    Q.getExamStepVariantConf(id).as(Q.examStepVariantConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"ExamStepVariantConf with id: $id not found")
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

  def getExamStepVariantConfByExamStepConfId(examStepConfId: Long): ExamStepVariantConf = db.run{ implicit c =>
    Q.getExamStepVariantConfByExamStepConfId(examStepConfId).as(Q.examStepVariantConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"No variants found for step conf id: $examStepConfId")
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
  }
  
  object ES {
    val table = "exam_step_confs"
    val id = "id"
    val examConfId = "exam_conf_id"
    val sequence = "sequence"
    val name = "name"
    val stepType = "step_type"
    val mistakesPerAttemptLimit = "mistakes_per_attempt_limit"
    val attemptsLimit = "attempts_limit"
    val hasToBeSubmitted = "has_to_be_submitted"
  }
  
  object ESV {
    val table = "exam_step_variant_confs"
    val id = "id"
    val examConfId = "exam_conf_id"
    val examStepConfId = "exam_step_conf_id"
    val dataSetConfId = "data_set_conf_id"
  }

  val examConfParser  = for {
    id <- long(E.id)
    name <- str(E.name)
    description <- str(E.description)
  } yield ExamConf(id, name, description)

  val examStepConfParser = for {
    id <- long(ES.id)
    examConfId <- long(ES.examConfId)
    sequence <- int(ES.sequence)
    name <- str(ES.name)
    stepType <- int(ES.stepType)
    mistakesPerAttemptLimit <- int(ES.mistakesPerAttemptLimit)
    attemptsLimit <- int(ES.attemptsLimit)
    hasToBeSubmitted <- bool(ES.hasToBeSubmitted)
  } yield ExamStepConf(id, examConfId, sequence, name, ExamStepType(stepType), mistakesPerAttemptLimit, attemptsLimit, hasToBeSubmitted)

  val examStepVariantConfParser = for {
    id <- long(ESV.id)
    examConfId <- long(ESV.examConfId)
    examStepConfId <- long(ESV.examStepConfId)
    dataSetConfId <- long(ESV.dataSetConfId)
  } yield ExamStepVariantConf(id, examStepConfId, examConfId, dataSetConfId)


  def createExamConf(ec: ExamConf) =
    SQL(s"INSERT INTO ${E.table} (${E.name}, ${E.description}) VALUES ({name}, {description})")
      .on("name" -> ec.name)
      .on("description" -> ec.description)

  def createExamStepConf(esc: ExamStepConf) =
    SQL(
      s"""INSERT INTO ${ES.table} (
         |${ES.examConfId},
         |${ES.sequence},
         |${ES.name},
         |${ES.stepType},
         |${ES.mistakesPerAttemptLimit},
         |${ES.attemptsLimit},
         |${ES.hasToBeSubmitted}
         |)
         |VALUES (
         |{examConfId},
         |{sequence},
         |{name},
         |{stepType},
         |{mistakesPerAttemptLimit},
         |{attemptsLimit},
         |{hasToBeSubmitted}
         |)
       """.stripMargin)
      .on("examConfId" -> esc.examConfId)
      .on("sequence" -> esc.sequence)
      .on("name" -> esc.name)
      .on("stepType" -> esc.stepType.id)
      .on("mistakesPerAttemptLimit" -> esc.mistakesPerAttemptLimit)
      .on("attemptsLimit" -> esc.attemptsLimit)
      .on("hasToBeSubmitted" -> esc.hasToBeSubmitted)

  def createExamStepVariantConf(esvc: ExamStepVariantConf) =
    SQL(
      s"""INSERT INTO ${ESV.table}
         |(${ESV.examConfId}, ${ESV.examStepConfId}, ${ESV.dataSetConfId})
         |VALUES ({examConfId}, {examStepConfId}, {dataSetConfId})
       """.stripMargin)
      .on("examConfId" -> esvc.examConfId)
      .on("examStepConfId" -> esvc.examStepConfId)
      .on("dataSetConfId" -> esvc.dataSetConfId)

  def getExamConf(id: Long) = SQL(s"SELECT * FROM ${E.table} WHERE ${E.id} = {id}").on("id" -> id)

  def getExamStepConf(id: Long) = SQL(s"SELECT * FROM ${ES.table} WHERE ${ES.id} = {id}").on("id" -> id)

  def getExamStepVariantConf(id: Long) = SQL(s"SELECT * FROM ${ESV.table} WHERE ${ESV.id} = {id}").on("id" -> id)

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

  def getExamStepVariantConfByExamStepConfId(examStepConfId: Long) =
    SQL(s"SELECT * FROM ${ESV.table} WHERE ${ESV.examStepConfId} = {examStepConfId}")
      .on("examStepConfId" -> examStepConfId)
  
}
