package edu.knuca.resmat.exam

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.core.{RingPlateProblemAnswer, RingPlateProblemInput, RingPlateSolver}
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.taskflow.TaskFlowQueries
import edu.knuca.resmat.http.FailedDependency
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext

case class ProblemConfWithVariants(problemConf: ProblemConf, variants: Seq[ProblemVariantConf])

case class NewProblemVariantConfDto(schemaUrl: String, inputVariableValues: Seq[ProblemInputVariableValue])

class ProblemService(val db: DatabaseService)(implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.http.JsonProtocol._
  import edu.knuca.resmat.exam.{ProblemQueries => Q}

  def getProblemConfById(id: Long): ProblemConf = db.run { implicit c =>
    Q.getProblemConfById(id).as(Q.problemConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Problem conf with id: $id not found!")
    )
  }

  def findProblemConfs(): Seq[ProblemConf] = db.run { implicit c =>
    Q.findProblemConfs.as(Q.problemConfParser.*)
  }

  def getProblemConfWithVariants(id: Long): ProblemConfWithVariants = {
    val pc = getProblemConfById(id)
    val variants = findProblemVariantConfsByProblemConfId(id)
    ProblemConfWithVariants(pc, variants)
  }

  def getProblemVariantConfById(id: Long): ProblemVariantConf = db.run { implicit c =>
    Q.getProblemVariantConfById(id).as(Q.problemVariantConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Problem variant conf with id: $id not found!")
    )
  }

  def findProblemVariantConfsByProblemConfId(problemConfId: Long): Seq[ProblemVariantConf] = db.run { implicit c =>
    Q.findProblemVariantConfsByProblemConfId(problemConfId).as(Q.problemVariantConfParser.*)
  }

  def createProblemConf(p: ProblemConf): ProblemConf = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createProblemConf(p).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert problem conf: $p")
    )
    getProblemConfById(insertedId)
  }

  def createProblemVariantConf(p: ProblemVariantConf): ProblemVariantConf = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createProblemVariantConf(p).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert problem conf: $p")
    )
    getProblemVariantConfById(insertedId)
  }

  def updateProblemVariantConf(id: Long, p: ProblemVariantConf): ProblemVariantConf = db.runTransaction { implicit c =>
    val affectedRows = Q.updateProblemVariantConf(id, p).executeUpdate()
    if(affectedRows != 1) {
      throw new RuntimeException(s"Failed to update problem variant conf by id: $id, affected rows: $affectedRows")
    }
    getProblemVariantConfById(id)
  }

  def recalculateProblemVariantConfs(problemConfId: Long): Unit = {
    val problemConf = getProblemConfById(problemConfId)
    val variantConfs = findProblemVariantConfsByProblemConfId(problemConfId)
    variantConfs.foreach(vc => {
      val calculatedData = new RingPlateSolver(problemConf.inputVariableConfs, vc.inputVariableValues).solve()
      updateProblemVariantConf(vc.id, vc.copy(calculatedData = calculatedData))
    })
  }

  def calculateAndCreateProblemVariantConf(p: NewProblemVariantConfDto, problemConfId: Long): ProblemVariantConf = {
    val problemConf = getProblemConfById(problemConfId)
    val calculatedData = new RingPlateSolver(problemConf.inputVariableConfs, p.inputVariableValues).solve()
    createProblemVariantConf(ProblemVariantConf(-1, problemConfId, p.schemaUrl, p.inputVariableValues, calculatedData))
  }

  def deleteProblemVariantConf(id: Long, force: Boolean) = db.runTransaction { implicit c =>
    val involvedTaskFlows = Q.findUserExamTaskFlowsByVariant(id).as(TaskFlowQueries.uetfParser.*)
    if(involvedTaskFlows.nonEmpty) {
      if(force) {
        UserExamQueries.deleteUserExamStepAttempts(involvedTaskFlows.map(_.stepAttemptId)).executeUpdate()
      } else {
        throw FailedDependency(s"Conflicting task flow ids: ${involvedTaskFlows.map(_.id)}")
      }
    }
    val affectedRows = Q.deleteProblemVariantConf(id).executeUpdate()
    if(affectedRows != 1) {
      throw new RuntimeException("Failed to delete ProblemVariantConf with id: " + id )
    }
  }

}

object ProblemQueries {

  import anorm.SqlParser.{int, long, str, bool}

  object P {
    val table = "problem_confs"
    val id = "id"
    val name = "name"
    val problemType = "problem_type"
    val inputVariableConfs = "input_variable_confs"
  }

  object PV {
    val table = "problem_variant_confs"
    val id = "id"
    val problemConfId = "problem_conf_id"
    val schemaUrl = "schema_url"
    val inputVariableValues = "input_variable_values"
    val calculatedData = "calculated_data"
  }

  val problemConfParser = for {
    id <- long(P.id)
    name <- str(P.name)
    problemType <- int(P.problemType)
    inputVariableConfs <- str(P.inputVariableConfs)
  } yield ProblemConf(id, name, ProblemType(problemType), decodeInputVariableConfs(inputVariableConfs))

  val problemVariantConfParser = for {
    id <- long(PV.id)
    problemConfId <- long(PV.problemConfId)
    schemaUrl <- str(PV.schemaUrl)
    inputVariableValues <- str(PV.inputVariableValues)
    calculatedData <- str(PV.calculatedData)
  } yield ProblemVariantConf(
    id, problemConfId, schemaUrl, decodeInputVariableValues(inputVariableValues), decodeCalculatedData(calculatedData)
  )

  def createProblemConf(p: ProblemConf) =
    SQL(
      s"""INSERT INTO ${P.table} (
         |${P.name},
         |${P.problemType},
         |${P.inputVariableConfs})
         |VALUES (
         |{name},
         |{problemType},
         |{inputVariableConfs})
       """.stripMargin)
      .on("name" -> p.name)
      .on("problemType" -> p.problemType.id)
      .on("inputVariableConfs" -> p.inputVariableConfs.asJson.toString)

  def getProblemConfById(id: Long) = SQL(s"SELECT * FROM ${P.table} WHERE ${P.id} = {id}").on("id" -> id)

  def findProblemConfs = SQL(s"SELECT * FROM ${P.table}")

  def createProblemVariantConf(pv: ProblemVariantConf) =
    SQL(
      s"""INSERT INTO ${PV.table} (
         |${PV.problemConfId},
         |${PV.schemaUrl},
         |${PV.inputVariableValues},
         |${PV.calculatedData})
         |VALUES (
         |{problemConfId},
         |{schemaUrl},
         |{inputVariableValues},
         |{calculatedData})
       """.stripMargin)
      .on("problemConfId" -> pv.problemConfId)
      .on("schemaUrl" -> pv.schemaUrl)
      .on("inputVariableValues" -> pv.inputVariableValues.asJson.toString)
      .on("calculatedData" -> pv.calculatedData.asJson.toString)

  def updateProblemVariantConf(id: Long, pv: ProblemVariantConf) =
    SQL(
      s"""UPDATE ${PV.table} SET
         |${PV.schemaUrl} = {schemaUrl},
         |${PV.inputVariableValues} = {inputVariableValues},
         |${PV.calculatedData} = {calculatedData}
         |WHERE id = {id}
       """.stripMargin)
      .on("id" -> id)
      .on("schemaUrl" -> pv.schemaUrl)
      .on("inputVariableValues" -> pv.inputVariableValues.asJson.toString)
      .on("calculatedData" -> pv.calculatedData.asJson.toString)

  def getProblemVariantConfById(id: Long) = SQL(s"SELECT * FROM ${PV.table} WHERE ${PV.id} = {id}").on("id" -> id)

  def findProblemVariantConfsByProblemConfId(problemConfId: Long) =
    SQL(s"SELECT * FROM ${PV.table} WHERE ${PV.problemConfId} = {problemConfId}").on("problemConfId" -> problemConfId)

  def deleteProblemVariantConf(id: Long) = SQL(s"DELETE FROM ${PV.table} WHERE ${PV.id} = {id}").on("id" -> id)

  def findUserExamTaskFlowsByVariant(problemVariantConfId: Long) =
    SQL(s"SELECT * FROM ${TaskFlowQueries.UETF.table} WHERE ${TaskFlowQueries.UETF.problemVariantConfId} = {problemVariantConfId}")
      .on("problemVariantConfId" -> problemVariantConfId)

  private def decodeInputVariableConfs(json: String): Seq[ProblemInputVariableConf] = {
    decode[Seq[ProblemInputVariableConf]](json).fold( e =>
      throw new RuntimeException(s"Failed to decode InputVariableConfs in json: $json", e),
      r => r
    )
  }

  private def decodeInputVariableValues(json: String): Seq[ProblemInputVariableValue] = {
    decode[Seq[ProblemInputVariableValue]](json).fold( e =>
      throw new RuntimeException(s"Failed to decode InputVariableValues in json: $json", e),
      r => r
    )
  }

  //todo switch to interface to allow to work with different problems
  private def decodeCalculatedData(json: String): RingPlateProblemAnswer = {
    decode[RingPlateProblemAnswer](json).fold( e =>
      throw new RuntimeException(s"Failed to decode CalculatedData in json: $json", e),
      r => r
    )
  }
}
