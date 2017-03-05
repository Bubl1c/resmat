package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.ExamStatus.ExamStatus
import edu.knuca.resmat.exam.ExamStepDataType.ExamStepDataType

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

trait StepData

case class UserExamDto(userExamId: Long, variant: Int, status: ExamStatus, currentStep: Option[UserExamStepPreviewDto], conf: ExamConf)
case class UserExamStepPreviewDto(sequence: Int, stepType: ExamStepDataType, description: String)
case class UserExamStepInfoDto(conf: ExamStepConf, attempts: Seq[UserExamStepAttempt])
case class UserExamStepAttemptDto(conf: ExamStepConf, info: UserExamStepAttempt, data: StepData)

case class NI(data: String = "not implemented") extends StepData

class ExamService(val db: DatabaseService)
                 (testSetExamService: TestSetExamService)
                 (implicit val executionContext: ExecutionContext) extends LazyLogging {

  private val examConfs: List[ExamConf] = List(
    ExamConf(1, "Exam1", "Exam1 description")
  )
  private val examStepConfs: List[ExamStepConf] = List(
    ExamStepConf(1, 1, 1, "Exam1Step1", ExamStepDataType.TestSet)
  )
  private val examStepVariantConfs: List[ExamStepVariantConf] = List( //figure out variants
    ExamStepVariantConf(1, 1, 1, 1)
  )

  //===============================================================
  //                      User specific data
  //===============================================================

  private val userExams: List[UserExam] = List(
    UserExam(1, 1, 1, 1, 1, ExamStatus.InProgress, None, None),
    UserExam(2, 1, 1, 1, -1, ExamStatus.Initial, None, None)
  )
  private val userExamStepAttempts: ListBuffer[UserExamStepAttempt] = ListBuffer(
    UserExamStepAttempt(1, 1, 1, 1, 0, 1, ExamStepStatus.Failed, 1, 1)
  )

  //===============================================================
  //                      Code
  //===============================================================

  def getUserExamById(userExamId: Long): Option[UserExamDto] = userExams.find(_.id == userExamId).map(mapToDto)

  def getUserExamsAvailableForUser(userId: Long): Seq[UserExamDto] = {
    val ues = userExams.filter(_.userId == userId)
    ues.map(mapToDto)
  }

  def getCurrentUserExam(userId: Long): Option[UserExamDto] = {
    val currentInProgress = userExams.find(ue => ue.userId == userId && ue.status == ExamStatus.InProgress)
    currentInProgress.fold{
      val any = userExams.find(ue => ue.userId == userId)
      any.map(mapToDto)
    }{ ue =>
      Some(mapToDto(ue))
    }
  }

  def getUserExamStepInfos(userExamId: Long): Seq[UserExamStepInfoDto] = {
    val userExamOpt = userExams.find(_.id == userExamId)
    if(userExamOpt.isDefined) {
      val userExam = userExamOpt.get
      val stepConfs = examStepConfs.filter(esc => esc.examConfId == userExam.examConfId)
      stepConfs.map(getStepInfo(userExam.userId, _))
    } else {
      Seq()
    }
  }

  def getUserExamStepInfo(userExamId: Long, sequence: Int): Option[UserExamStepInfoDto] = {
    val steps = getUserExamStepInfos(userExamId)
    steps.find(_.conf.sequence == sequence)
  }

  def getUserExamStepCurrentAttempt(userExamId: Long, sequence: Int): Option[UserExamStepAttemptDto] = {
    val userExam = userExams.find(_.id == userExamId).getOrElse(
      throw new RuntimeException(s"Exam with id: $userExamId not found.")
    )
    val examStepConf = examStepConfs.find(esc => esc.examConfId == userExam.examConfId && esc.sequence == sequence).getOrElse(
      throw new RuntimeException(s"Exam ($userExamId) step with sequence: $sequence not found.")
    )

    val existingAttempts = getUserExamStepAttempts(userExam.userId, examStepConf.id)

    val successAttemptOpt = existingAttempts.find(_.status == ExamStepStatus.Success)
    if(successAttemptOpt.isDefined) {
      None
    } else {
      userExamStepAttempts.find(_.status == ExamStepStatus.NotSubmitted).fold{
        val newAttempt = createAttempt(userExam, examStepConf)
        getAttemptDto(newAttempt, examStepConf)
      }{ notSubmittedAttempt =>
        getAttemptDto(notSubmittedAttempt, examStepConf)
      }
    }
  }

  private def createStepAttempt(attempt: UserExamStepAttempt): UserExamStepAttempt = {
    val existingAttempts = getUserExamStepAttempts(attempt.userId, attempt.examStepConfId)
    val nextStepAttemptId = if(userExamStepAttempts.nonEmpty) userExamStepAttempts.last.id + 1 else 1
    val withId = attempt.copy(id = nextStepAttemptId, attemptNumber = existingAttempts.size + 1)
    userExamStepAttempts += withId
    withId
  }

  private def getUserExamStepAttempts(userId: Long, stepConfId: Long): Seq[UserExamStepAttempt] =
    userExamStepAttempts.filter(sa => sa.userId == userId && sa.examStepConfId == stepConfId)

  private def createAttempt(userExam: UserExam, examStepConf: ExamStepConf): UserExamStepAttempt = {
    examStepConf.stepType match {
      case ExamStepDataType.TestSet =>
        createNewTestSetAttempt(userExam, examStepConf.id)
      case ExamStepDataType.TaskFlow =>
        null
      case ExamStepDataType.Results =>
        null
      case anyOther =>
        throw new RuntimeException(s"Unhandled ExamStepDataType: $anyOther")
    }
  }

  private def createNewTestSetAttempt(userExam: UserExam, examStepConfId: Long): UserExamStepAttempt = {
    val variantConf = examStepVariantConfs.find(_.examStepConfId == examStepConfId).getOrElse( //todo Consider to choose unused variant. If all used - not like at previous attempt
      throw new RuntimeException(s"No variants found for step conf id: $examStepConfId")
    )
    val testSetConf = testSetExamService.getTestSetConf(variantConf.dataSetConfId).getOrElse(
      throw new RuntimeException(s"TestSetConf with id: ${variantConf.dataSetConfId} not found.")
    )

    val (newTestSet, newTestSetTests) =
      testSetExamService.createTestSetWithTests(UserExamStepAttemptTestSet(-1, userExam.id, examStepConfId, testSetConf.id))

    val newTestSetStepAttempt =
      createStepAttempt(
        UserExamStepAttempt(-1, userExam.userId, userExam.id, examStepConfId, 0, -1, ExamStepStatus.NotSubmitted, variantConf.id, newTestSet.id)
      )

    newTestSetStepAttempt
  }

  private def getAttemptDto(attempt: UserExamStepAttempt, examStepConf: ExamStepConf): Option[UserExamStepAttemptDto] = {
    examStepConf.stepType match {
      case ExamStepDataType.TestSet =>
        val testSetDto = testSetExamService.getTestSetDto(attempt.dataSetId).getOrElse(
          throw new RuntimeException(s"Test set data not found. Attempt: $attempt")
        )
        Some(UserExamStepAttemptDto(examStepConf, attempt, testSetDto))
      case ExamStepDataType.TaskFlow =>
        Some(UserExamStepAttemptDto(null, null, NI()))
      case ExamStepDataType.Results =>
        Some(UserExamStepAttemptDto(null, null, NI()))
      case anyOther =>
        logger.error(s"Unhandled ExamStepDataType: $anyOther")
        None
    }
  }

  private def mapToDto(ue: UserExam): UserExamDto = {
    val configuration = examConfs.find(_.id == ue.examConfId).getOrElse(
      throw new RuntimeException(s"Cannot find exam configuration by id: ${ue.examConfId}")
    )
    val currentStepOpt = examStepConfs.find(_.id == ue.currentStepId).map( cs =>
      UserExamStepPreviewDto(cs.sequence, cs.stepType, cs.name)
    )
    UserExamDto(ue.id, ue.variant, ue.status, currentStepOpt, configuration)
  }

  private def getStepInfo(userId: Long, stepConf: ExamStepConf): UserExamStepInfoDto = {
    val stepAttempts = userExamStepAttempts.filter(ues => ues.userId == userId && ues.examStepConfId == stepConf.id)
    UserExamStepInfoDto(stepConf, stepAttempts)
  }

}

object ExamQueries {

}