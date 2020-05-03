package edu.knuca.resmat.exam

import java.sql.Connection

import anorm.{BatchSql, NamedParameter, SQL}
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.taskflow.TaskFlowConfAndExamService
import edu.knuca.resmat.exam.testset.TestSetExamService
import edu.knuca.resmat.tests.TestConfService
import edu.knuca.resmat.user.AuthenticatedUser
import edu.knuca.resmat.utils.CollectionUtils

import scala.concurrent.ExecutionContext
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

class ExamConfService(val db: DatabaseService, testConfsService: TestConfService, taskFlowExamService: TaskFlowConfAndExamService)
                     (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.exam.{ExamQueries => Q}

  // todo optimise to save all steps at once
  def createExamConfWithSteps(ec: ExamConfCreateDto, ownerUserIdOpt: Option[Long] = None): ExamConfWithStepsDto = db.runTransaction{ implicit c =>
    val createdExamConfId = createExamConfTransact(ec.examConf)
    val steps = ec.stepConfs.map(esc => createExamStepConfTransact(createdExamConfId, esc))
    ownerUserIdOpt.foreach(ownerUserId => {
      Q.grantAccessToExamConfs(ownerUserId, Set(createdExamConfId)).execute()
    })
    getExamConfDtoTransact(createdExamConfId)
  }

  def updateExamConfWithSteps(id: Long, ec: ExamConfUpdateDto): ExamConfWithStepsDto = db.runTransaction{ implicit c =>
    updateExamConfTransact(id, ec.examConf)
    val currentSteps = findExamStepConfsByExamConfId(id)
    val diff = CollectionUtils.diff[ExamStepConf, ExamStepConfUpdateDto](
      currentSteps,
      ec.stepConfs,
      (a, b) => a.id == b.examStepConf.id
    )
    val created = diff.added.map(esc =>
      createExamStepConfTransact(
        id,
        ExamStepConfCreateDto(
          esc.examStepConf,
          esc.stepDataConf.getOrElse(
            throw new IllegalArgumentException("New exam conf step must have step data defined")
          )
        )
      )
    )
    diff.same.foreach(esc => updateExamStepConfTransact(esc.examStepConf.id, esc))
    diff.removed.foreach(esc => deleteExamStepConfDataTransact(esc.id))

    getExamConfDtoTransact(id)
  }

  def deleteExamConf(examConfId: Long): Unit = db.runTransaction{ implicit c =>
    val dto = getExamConfDto(examConfId)
    dto.stepConfs.foreach(esc => deleteExamStepConfDataTransact(esc.id))
    Q.deleteExamConf(examConfId).executeUpdate()
  }

  private def createExamConfTransact(ec: ExamConf)(implicit c: Connection): Long = {
    val insertedIdOpt: Option[Long] = Q.createExamConf(ec).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert exam conf: $ec")
    )
    insertedId
  }

  private def updateExamConfTransact(id: Long, ec: ExamConf)(implicit c: Connection): ExamConf = {
    val rowsUpdated = Q.updateExamConf(id, ec).executeUpdate()
    if (rowsUpdated != 1) throw new RuntimeException(s"Failed to update exam conf with id $id, rows updated: " + rowsUpdated)
    getExamConfTransact(id)
  }

  def setArchivedForExamConf(id: Long, isArchived: Boolean): Unit = db.run { implicit c =>
    val ec = getExamConfTransact(id)
    updateExamConfTransact(id, ec.copy(isArchived = isArchived))
  }

  private def createExamStepConfTransact(examConfId: Long, esc: ExamStepConfCreateDto)(implicit c: Connection): Long = {
    val stepDataSet: ExamStepConfDataSet = esc.examStepConf.stepType match {
      case ExamStepType.TestSet =>
        val data: TestSetConfDto = esc.stepDataConf.asInstanceOf[TestSetConfDto]
        val testSetConfId = testConfsService.createTestSetConfTransact(data.testSetConf)
        testConfsService.createTestSetConfTestGroupsTransact(data.testGroups.map(_.copy(testSetConfId = testSetConfId)))
        ExamStepTestSetDataSet(testSetConfId)
      case ExamStepType.TaskFlow =>
        val data = esc.stepDataConf.asInstanceOf[TaskFlowConfDto]
        val taskFlowConfId = taskFlowExamService.createTaskFlowConfWithStepsTransact(data)
        ExamStepTaskFlowDataSet(taskFlowConfId, data.taskFlowConf.problemConfId)
      case ExamStepType.Results => ExamStepResultsDataSet
    }
    val insertedIdOpt: Option[Long] = Q.createExamStepConf(examConfId, esc.examStepConf.copy(dataSet = stepDataSet)).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert exam step conf: $esc")
    )
    insertedId
  }

  private def updateExamStepConfTransact(id: Long, esc: ExamStepConfUpdateDto)(implicit c: Connection): Unit = {
    val stepDataSet: ExamStepConfDataSet = esc.stepDataConf.map(stepDataConf => esc.examStepConf.stepType match {
      case ExamStepType.TestSet =>
        val data: TestSetConfDto = stepDataConf.asInstanceOf[TestSetConfDto]
        testConfsService.updateTestSetConfTransact(data.testSetConf.id, data.testSetConf)
        testConfsService.deleteTestSetConfTestGroupsTransact(data.testSetConf.id)
        testConfsService.createTestSetConfTestGroupsTransact(data.testGroups)
        ExamStepTestSetDataSet(data.testSetConf.id)
      case ExamStepType.TaskFlow =>
        val data = stepDataConf.asInstanceOf[TaskFlowConfDto]
        taskFlowExamService.updateTaskFlowConfWithStepsTransact(data.taskFlowConf.id, data)
        ExamStepTaskFlowDataSet(data.taskFlowConf.id, data.taskFlowConf.problemConfId)
      case ExamStepType.Results => ExamStepResultsDataSet
    }).getOrElse(esc.examStepConf.dataSet)
    val rowsUpdated = Q.updateExamStepConf(id, esc.examStepConf.copy(dataSet = stepDataSet)).executeUpdate()
    if (rowsUpdated != 1) throw new RuntimeException(s"Failed to update exam step conf with id $id, rows updated: " + rowsUpdated)
  }

  private def deleteExamStepConfDataTransact(examStepConfId: Long)(implicit c: Connection): Unit = {
    val esc = getExamStepConfTransact(examStepConfId)
    esc.stepType match {
      case ExamStepType.TestSet =>
        val data = esc.dataSet.asInstanceOf[ExamStepTestSetDataSet]
        testConfsService.deleteTestSetConfTransact(data.testSetConfId)
      case ExamStepType.TaskFlow =>
        val data = esc.dataSet.asInstanceOf[ExamStepTaskFlowDataSet]
        taskFlowExamService.deleteTaskFlowConfTransact(data.taskFlowConfId)
      case ExamStepType.Results => //do nothing
      case _ => throw new IllegalArgumentException(s"Invalid exam step type ${esc.stepType}")
    }
  }

  def getExamConf(id: Long): ExamConf = db.run{ implicit c =>
    getExamConfTransact(id)
  }

  private def getExamConfTransact(id: Long)(implicit c: Connection): ExamConf = {
    Q.getExamConf(id).as(Q.examConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Cannot find exam conf by id: $id")
    )
  }

  def findExamConfs(isArchived: Option[Boolean], onlyAccessible: Boolean)(implicit user: AuthenticatedUser): Seq[ExamConf] = db.run{ implicit c =>
    val accessibleForUserId = if (onlyAccessible) { Some(user.id) } else None 
    Q.findExamConfs(isArchived, accessibleForUserId).as(Q.examConfParser.*)
  }

  def getAccessToExamConfs(userId: Long): UserExamConfAccessDto = db.run{ implicit c =>
    val examConfIds = Q.getAccessToExamConfs(userId).as(Q.userExamConfAccessParser.*)
    UserExamConfAccessDto(userId, examConfIds.toSet)
  }

  def setUserExamConfAccess(dto: UserExamConfAccessDto): Unit = db.runTransaction { implicit c =>
    Q.removeAccessToExamConfs(dto.userId).execute()
    if (dto.examConfIds.nonEmpty) {
      Q.grantAccessToExamConfs(dto.userId, dto.examConfIds).execute()
    }
  }

  def getExamConfDto(id: Long): ExamConfWithStepsDto = {
    val examConf = getExamConf(id)
    val stepConfs = findExamStepConfsByExamConfId(id)
    ExamConfWithStepsDto(examConf, stepConfs)
  }

  def getExamConfDtoTransact(id: Long)(implicit c: Connection): ExamConfWithStepsDto = {
    val examConf = getExamConfTransact(id)
    val stepConfs = findExamStepConfsByExamConfIdTransact(id)
    ExamConfWithStepsDto(examConf, stepConfs)
  }

  def getExamStepConf(id: Long): ExamStepConf = db.run { implicit c =>
    getExamStepConfTransact(id)
  }

  private def getExamStepConfTransact(id: Long)(implicit c: Connection): ExamStepConf = {
    Q.getExamStepConf(id).as(Q.examStepConfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Exam step conf with id: $id not found.")
    )
  }

  def findExamStepConfsByExamConfId(examConfId: Long): Seq[ExamStepConf] = db.run{ implicit c =>
    findExamStepConfsByExamConfIdTransact(examConfId)
  }

  def findExamStepConfsByExamConfIdTransact(examConfId: Long)(implicit c: Connection): Seq[ExamStepConf] = {
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

  object EC {
    val table = "exam_confs"
    val id = "id"
    val name = "name"
    val description = "description"
    val maxScore = "max_score"
    val isArchived = "is_archived"
  }

  object ESC {
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

  object UsECs {
    val table = "users_exam_confs"
    val userId = "user_id"
    val examConfId = "exam_conf_id"
  }
  val userExamConfAccessParser = for {
    examConfId <- long(UsECs.examConfId)
  } yield examConfId

  val examConfParser  = for {
    id <- long(EC.id)
    name <- str(EC.name)
    description <- str(EC.description)
    maxScore <- int(EC.maxScore)
  } yield ExamConf(id, name, description, maxScore)

  val examStepConfParser = for {
    id <- long(ESC.id)
    examConfId <- long(ESC.examConfId)
    sequence <- int(ESC.sequence)
    name <- str(ESC.name)
    stepType <- int(ESC.stepType)
    mistakesPerAttemptLimit <- int(ESC.mistakesPerAttemptLimit)
    mistakeValue <- int(ESC.mistakeValuePercents)
    attemptsLimit <- int(ESC.attemptsLimit)
    attemptValue <- int(ESC.attemptValuePercents)
    maxScore <- int(ESC.maxScore)
    dataSet <- str(ESC.dataSet)
    hasToBeSubmitted <- bool(ESC.hasToBeSubmitted)
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
    SQL(s"INSERT INTO ${EC.table} (${EC.name}, ${EC.description}, ${EC.maxScore}) VALUES ({name}, {description}, {maxScore})")
      .on("name" -> ec.name)
      .on("description" -> ec.description)
      .on("maxScore" -> ec.maxScore)

  def getAccessToExamConfs(userId: Long) = {
    SQL(s"SELECT * FROM ${UsECs.table} WHERE ${UsECs.userId} = {userId}")
      .on("userId" -> userId)
  }
  
  def grantAccessToExamConfs(userId: Long, ecIds: Set[Long]): BatchSql = {
    val values = ecIds.toSeq.map( ecId =>
      Seq[NamedParameter](
        "userId" -> userId,
        "examConfId" -> ecId
      )
    )
    BatchSql(
      s"""INSERT INTO ${UsECs.table} (
         |${UsECs.userId},
         |${UsECs.examConfId}
         |) VALUES (
         |{userId},
         |{examConfId}
         |)""".stripMargin,
      values.head,
      values.tail : _*
    )
  }

  def removeAccessToExamConfs(userId: Long) = {
    SQL(s"DELETE FROM ${UsECs.table} WHERE ${UsECs.userId} = {userId}")
      .on("userId" -> userId)
  }

  def updateExamConf(id: Long, ec: ExamConf) =
    SQL(
      s"""UPDATE ${EC.table} SET
         |${EC.name} = {name},
         |${EC.description} = {description},
         |${EC.maxScore} = {maxScore},
         |${EC.isArchived} = {isArchived}
         |WHERE id = {id}
       """.stripMargin)
      .on("id" -> id)
      .on("name" -> ec.name)
      .on("description" -> ec.description)
      .on("maxScore" -> ec.maxScore)
      .on("isArchived" -> ec.isArchived)

  def deleteExamConf(id: Long) =
    SQL(s"DELETE FROM ${EC.table} WHERE id = {id}")
      .on("id" -> id)

  def createExamStepConf(examConfId: Long, esc: ExamStepConf) =
    SQL(
      s"""INSERT INTO ${ESC.table} (
         |${ESC.examConfId},
         |${ESC.sequence},
         |${ESC.name},
         |${ESC.stepType},
         |${ESC.mistakesPerAttemptLimit},
         |${ESC.mistakeValuePercents},
         |${ESC.attemptsLimit},
         |${ESC.attemptValuePercents},
         |${ESC.maxScore},
         |${ESC.dataSet},
         |${ESC.hasToBeSubmitted}
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
      .on("examConfId" -> examConfId)
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

  def updateExamStepConf(id: Long, esc: ExamStepConf) =
    SQL(
      s"""UPDATE ${ESC.table} SET
         |${ESC.sequence} = {sequence},
         |${ESC.name} = {name},
         |${ESC.stepType} = {stepType},
         |${ESC.mistakesPerAttemptLimit} = {mistakesPerAttemptLimit},
         |${ESC.mistakeValuePercents} = {mistakeValuePercents},
         |${ESC.attemptsLimit} = {attemptsLimit},
         |${ESC.attemptValuePercents} = {attemptValuePercents},
         |${ESC.maxScore} = {maxScore},
         |${ESC.dataSet} = {dataSet},
         |${ESC.hasToBeSubmitted} = {hasToBeSubmitted}
         |WHERE id = {id}
       """.stripMargin)
      .on("id" -> id)
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

  def getExamConf(id: Long) = SQL(s"SELECT * FROM ${EC.table} WHERE ${EC.id} = {id}").on("id" -> id)

  def findExamConfs(isArchived: Option[Boolean], accessibleForUserId: Option[Long]) = {
    val archivedCondition = isArchived
      .map(ia => s"WHERE ${EC.isArchived} IS $ia")
      .getOrElse(s"WHERE ${EC.isArchived} IS FALSE")
    val accessibleJoin = accessibleForUserId
      .map(userId => s"JOIN ${UsECs.table} ON ${UsECs.table}.${UsECs.examConfId} = ${EC.table}.${EC.id} AND ${UsECs.table}.${UsECs.userId} = $userId")
      .getOrElse("")
    SQL(
      s"SELECT ${EC.table}.* FROM ${EC.table} $accessibleJoin $archivedCondition"
    )
  }

  def getExamStepConf(id: Long) = SQL(s"SELECT * FROM ${ESC.table} WHERE ${ESC.id} = {id}").on("id" -> id)

  def findExamStepConfsByExamConfId(examConfId: Long) =
    SQL(s"SELECT * FROM ${ESC.table} WHERE ${ESC.examConfId} = {examConfId}")
      .on("examConfId" -> examConfId)

  def findSubmittableExamStepConf(examConfId: Long) =
    SQL(s"SELECT * FROM ${ESC.table} WHERE ${ESC.examConfId} = {examConfId} AND ${ESC.hasToBeSubmitted} IS TRUE")
      .on("examConfId" -> examConfId)

  def getExamStepConfByExamConfIdAndSequence(examConfId: Long, stepSequence: Int) =
    SQL(s"SELECT * FROM ${ESC.table} WHERE ${ESC.examConfId} = {examConfId} AND ${ESC.sequence} = {sequence}")
      .on("examConfId" -> examConfId)
      .on("sequence" -> stepSequence)

}
