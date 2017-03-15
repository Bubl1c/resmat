package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.ExamStatus.ExamStatus
import edu.knuca.resmat.exam.ExamStepType.ExamStepType

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

trait StepDataDto

case class UserExamDto(userExam: UserExam, currentStepPreview: Option[UserExamStepPreviewDto], examConf: ExamConf)
case class UserExamStepPreviewDto(sequence: Int, stepType: ExamStepType, description: String)
case class UserExamStepInfoDto(stepConf: ExamStepConf, attempts: Seq[UserExamStepAttempt])
case class UserExamStepAttemptDto(stepConf: ExamStepConf, attempt: UserExamStepAttempt, stepData: StepDataDto)

case class NI(data: String = "not implemented") extends StepDataDto

class ExamService(val db: DatabaseService)
                 (testSetExamService: TestSetExamService, taskFlowExamService: TaskFlowExamService)
                 (implicit val executionContext: ExecutionContext) extends LazyLogging {

  private val examConfs: List[ExamConf] = List(
    ExamConf(1, "Exam1", "Exam1 description")
  )
  private val examStepConfs: List[ExamStepConf] = List(
    ExamStepConf(1, 1, 1, "Exam1 Step1 Test Set", ExamStepType.TestSet, 5, 3),
    ExamStepConf(2, 1, 2, "Exam1 Step2 Task Flow", ExamStepType.TaskFlow, -1, -1),
    ExamStepConf(3, 1, 3, "Exam1 Step3 Results", ExamStepType.Results, -1, -1)
  )
  private val examStepVariantConfs: List[ExamStepVariantConf] = List( //figure out variants
    ExamStepVariantConf(1, 1, 1, 1),
    ExamStepVariantConf(2, 2, 1, 1),
    ExamStepVariantConf(3, 3, 1, 1)
  )

  //===============================================================
  //                      User specific data
  //===============================================================

  private val userExams: ListBuffer[UserExam] = ListBuffer(
    UserExam(1, 1, 1, 2, ExamStatus.InProgress, None, None),
    UserExam(2, 1, 1, -1, ExamStatus.Initial, None, None)
  )
  private val userExamStepAttempts: ListBuffer[UserExamStepAttempt] = ListBuffer(
//    UserExamStepAttempt(1, 1, 1, 1, 0, 1, ExamStepStatus.Failed, 1),
//    UserExamStepAttempt(2, 1, 1, 2, 3, 1, ExamStepStatus.NotSubmitted, 2),
//    UserExamStepAttempt(3, 1, 1, 3, 0, 1, ExamStepStatus.NotSubmitted, 3)
  )

  //===============================================================
  //                      User - exam
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

  def updateUserExam(userExam: UserExam): UserExam = {
    val userExamIndex = userExams.indexWhere(_.id == userExam.id)
    userExams.update(userExamIndex, userExam)
    userExam
  }

  private def mapToDto(ue: UserExam): UserExamDto = {
    val configuration = examConfs.find(_.id == ue.examConfId).getOrElse(
      throw new RuntimeException(s"Cannot find exam configuration by id: ${ue.examConfId}")
    )
    val currentStepOpt = examStepConfs.find(_.id == ue.currentStepConfId).map(cs =>
      UserExamStepPreviewDto(cs.sequence, cs.stepType, cs.name)
    )
    UserExamDto(ue, currentStepOpt, configuration)
  }

  //===============================================================
  //                      User - exam - step
  //===============================================================

  def getUserExamCurrentStepWithAttemptData(userExamId: Long): Option[UserExamStepAttemptDto] = {
    val userExam = userExams.find(_.id == userExamId).getOrElse(
      throw new RuntimeException(s"Exam with id: $userExamId not found.")
    )
    val examStepConf = examStepConfs.find(_.id == userExam.currentStepConfId).getOrElse(
      throw new RuntimeException(s"Exam ($userExamId) step with id: ${userExam.currentStepConfId} not found.")
    )
    if(userExam.status == ExamStatus.Success || userExam.status == ExamStatus.Failed) {
      throw new IllegalStateException(s"Exam has been already finished with status: ${userExam.status}")
    }
    getUserExamStepCurrentAttempt(userExam.id, examStepConf.sequence)
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
    steps.find(_.stepConf.sequence == sequence)
  }

  //todo make this method return uncompleted parts
  def submitStep(userExamId: Long, stepSequence: Int): Boolean = {
    val userExam = userExams.find(_.id == userExamId).getOrElse(
      throw new RuntimeException(s"Exam with id: $userExamId not found.")
    )
    val examStepConf = examStepConfs.find(esc => esc.examConfId == userExam.examConfId && esc.sequence == stepSequence).getOrElse(
      throw new RuntimeException(s"Exam ($userExamId) step with sequence: $stepSequence not found.")
    )
    val allStepAttempts = getUserExamStepAttempts(userExam.userId, examStepConf.id)

    val notSubmittedAttempts = allStepAttempts.filter(_.status == ExamStepStatus.NotSubmitted)
    val successAttempts = allStepAttempts.filter(_.status == ExamStepStatus.Success)

    if(successAttempts.nonEmpty || notSubmittedAttempts.size != 1) {
      throw new IllegalStateException(s"When submitting user exam id: $userExamId step sequence: $stepSequence " +
        s"success attempts: $successAttempts, not submitted attempts: $notSubmittedAttempts")
    }

    val stepAttempt = notSubmittedAttempts.head
    if(isStepCompleted(stepAttempt, examStepConf)) {
      updateStepAttempt(stepAttempt.copy(status = ExamStepStatus.Success))
      //Change exam's current step to next or mark exam as Successful if no more steps
      examStepConfs.find(esc => esc.examConfId == userExam.examConfId && esc.sequence == stepSequence + 1).fold(
        updateUserExam(userExam.copy(status = ExamStatus.Success))
      ){ nextStepConf =>
        updateUserExam(userExam.copy(currentStepConfId = nextStepConf.id))
      }
      true
    } else {
      false
    }
  }

  def isStepCompleted(stepAttempt: UserExamStepAttempt, examStepConf: ExamStepConf): Boolean = {
    examStepConf.stepType match {
      case ExamStepType.TestSet =>
        val stepAttemptTestSet = testSetExamService.getTestSetByAttemptId(stepAttempt.id).getOrElse(
          throw new RuntimeException(s"Test set for step attempt with id: ${stepAttempt.id} not found!")
        )
        val notCompletedTestConfs = testSetExamService.getNotCompletedTestConfsInTestSet(stepAttemptTestSet.id)
        notCompletedTestConfs.isEmpty
      case ExamStepType.TaskFlow =>
        val notCompletedTaskFlowSteps = taskFlowExamService.getNotCompletedTaskFlowSteps(stepAttempt.id)
        notCompletedTaskFlowSteps.isEmpty
      case ExamStepType.Results =>
        true
      case anyOther =>
        throw new IllegalArgumentException(s"Unhandled ExamStepDataType: $anyOther")
    }
  }

  private def chooseAvailableVariant(examStepConfId: Long): ExamStepVariantConf = {
    val variantConf = examStepVariantConfs.find(_.examStepConfId == examStepConfId).getOrElse( //todo Consider to choose unused variant. If all used - not like at previous attempt
      throw new RuntimeException(s"No variants found for step conf id: $examStepConfId")
    )
    variantConf
  }

  private def getStepInfo(userId: Long, stepConf: ExamStepConf): UserExamStepInfoDto = {
    val stepAttempts = userExamStepAttempts.filter(ues => ues.userId == userId && ues.examStepConfId == stepConf.id)
    UserExamStepInfoDto(stepConf, stepAttempts)
  }

  //===============================================================
  //                      User - exam - step  - attempt
  //===============================================================

  def getUserExamStepAttempts(userExamId: Long, sequence: Int): Seq[UserExamStepAttemptDto] = {
    val userExam = userExams.find(_.id == userExamId).getOrElse(
      throw new RuntimeException(s"Exam with id: $userExamId not found.")
    )
    val examStepConf = examStepConfs.find(esc => esc.examConfId == userExam.examConfId && esc.sequence == sequence).getOrElse(
      throw new RuntimeException(s"Exam ($userExamId) step with sequence: $sequence not found.")
    )

    val attempts = getUserExamStepAttempts(userExam.userId, examStepConf.id)

    attempts.flatMap(getAttemptDto(_, examStepConf))
  }

  def getUserExamStepCurrentAttempt(userExamId: Long, stepSequence: Int): Option[UserExamStepAttemptDto] = {
    val userExam = userExams.find(_.id == userExamId).getOrElse(
      throw new RuntimeException(s"Exam with id: $userExamId not found.")
    )
    val examStepConf = examStepConfs.find(esc => esc.examConfId == userExam.examConfId && esc.sequence == stepSequence).getOrElse(
      throw new RuntimeException(s"Exam ($userExamId) step with sequence: $stepSequence not found.")
    )

    if(userExam.status == ExamStatus.Success || userExam.status == ExamStatus.Failed) {
      throw new IllegalStateException(s"Exam has been already finished with status: ${userExam.status}")
    }

    val existingAttempts = getUserExamStepAttempts(userExam.userId, examStepConf.id)

    val successAttemptOpt = existingAttempts.find(_.status == ExamStepStatus.Success)
    if(successAttemptOpt.isDefined) {
      None
    } else {
      existingAttempts.find(_.status == ExamStepStatus.NotSubmitted).fold{
        val newAttempt = createAttempt(userExam, examStepConf)
        getAttemptDto(newAttempt, examStepConf)
      }{ notSubmittedAttempt =>
        getAttemptDto(notSubmittedAttempt, examStepConf)
      }
    }
  }

  def updateAttemptData(userExam: UserExam,
                        examStepConf: ExamStepConf,
                        currentStepAttempt: UserExamStepAttempt,
                        allStepAttempts: Seq[UserExamStepAttempt],
                        mistakesAmount: Int,
                        isCorrectAnswer: Boolean) = {
    val updatedMistakesAmount = currentStepAttempt.mistakesAmount + mistakesAmount

    //If mistakes limit exceeded - mark attempt as failed. -1 means infinite amount
    if(updatedMistakesAmount > examStepConf.mistakesPerAttemptLimit && examStepConf.mistakesPerAttemptLimit != -1) {
      updateStepAttempt(currentStepAttempt.copy(mistakesAmount = updatedMistakesAmount, status = ExamStepStatus.Failed))
      //If step attempts limit exceeded - mark exam as failed. -1 means infinite amount
      if(allStepAttempts.size == examStepConf.attemptsLimit && examStepConf.attemptsLimit != -1) {
        updateUserExam(userExam.copy(status = ExamStatus.Failed))
      }
    } else if(updatedMistakesAmount > currentStepAttempt.mistakesAmount){
      updateStepAttempt(currentStepAttempt.copy(mistakesAmount = updatedMistakesAmount))
    }
  }

  def updateStepAttempt(stepAttempt: UserExamStepAttempt): UserExamStepAttempt = {
    val stepAttemptIndex = userExamStepAttempts.indexWhere(_.id == stepAttempt.id)
    userExamStepAttempts.update(
      stepAttemptIndex,
      stepAttempt
    )
    stepAttempt
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
    val variantConf = chooseAvailableVariant(examStepConf.id)

    val newAttempt =
      createStepAttempt(
        UserExamStepAttempt(-1, userExam.userId, userExam.id, examStepConf.id, 0, -1, ExamStepStatus.NotSubmitted, variantConf.id)
      )

    //todo handle case when data set is not created - rollback attempt creation
    examStepConf.stepType match {
      case ExamStepType.TestSet =>
        createNewTestSetForAttempt(userExam, newAttempt, variantConf)
      case ExamStepType.TaskFlow =>
        createTaskFlowForAttempt(userExam, newAttempt, variantConf)
      case ExamStepType.Results =>
        null
      case anyOther =>
        throw new IllegalArgumentException(s"Unhandled ExamStepDataType: $anyOther")
    }

    newAttempt
  }

  private def getAttemptDto(attempt: UserExamStepAttempt, examStepConf: ExamStepConf): Option[UserExamStepAttemptDto] = {
    examStepConf.stepType match {
      case ExamStepType.TestSet =>
        val testSetDto = testSetExamService.getTestSetDto(attempt.id).getOrElse(
          throw new RuntimeException(s"Test set data not found. Attempt: $attempt")
        )
        Some(UserExamStepAttemptDto(examStepConf, attempt, testSetDto))
      case ExamStepType.TaskFlow =>
        val taskFlowDto = taskFlowExamService.getTaskFlowDto(attempt.id).getOrElse(
          throw new RuntimeException(s"Task flow data not found. Attempt: $attempt")
        )
        Some(UserExamStepAttemptDto(examStepConf, attempt, taskFlowDto))
      case ExamStepType.Results =>
        Some(UserExamStepAttemptDto(examStepConf, attempt, NI()))
      case anyOther =>
        throw new IllegalArgumentException(s"Unhandled ExamStepDataType: $anyOther")
    }
  }

  //===============================================================
  //                      Test - set
  //===============================================================

  def verifyTestSetAnswer(userExamId: Long,
                          stepSequence: Int,
                          stepAttemptId: Long,
                          testId: Long,
                          submittedOptions: Seq[Long]): Option[VerifiedTestAnswerDto] = {
    val userExam = userExams.find(_.id == userExamId).getOrElse(
      throw new RuntimeException(s"Exam with id: $userExamId not found.")
    )
    val examStepConf = examStepConfs.find(esc => esc.examConfId == userExam.examConfId && esc.sequence == stepSequence).getOrElse(
      throw new RuntimeException(s"Exam ($userExamId) step with sequence: $stepSequence not found.")
    )
    val allStepAttempts = getUserExamStepAttempts(userExam.userId, examStepConf.id)
    val currentStepAttempt = allStepAttempts.find(_.id == stepAttemptId).getOrElse(
      throw new IllegalArgumentException(s"Step attempt with id: $stepAttemptId not found!")
    )
    val stepAttemptTestSet = testSetExamService.getTestSetByAttemptId(currentStepAttempt.id).getOrElse(
      throw new RuntimeException(s"Test set for step attempt with id: ${currentStepAttempt.id} not found!")
    )

    testSetExamService.verifyTestSetTestAnswer(stepAttemptTestSet.id, TestAnswerDto(testId, submittedOptions)).map{ va =>
      updateAttemptData(userExam, examStepConf, currentStepAttempt, allStepAttempts, va.mistakesAmount, va.isCorrectAnswer)
      va
    }
  }

  private def createNewTestSetForAttempt(userExam: UserExam,
                                         attempt: UserExamStepAttempt,
                                         variantConf: ExamStepVariantConf): UserExamStepAttemptTestSet = {
    val testSetConf = testSetExamService.getTestSetConf(variantConf.dataSetConfId).getOrElse(
      throw new RuntimeException(s"TestSetConf with id: ${variantConf.dataSetConfId} not found.")
    )

    val (newTestSet, newTestSetTests) =
      testSetExamService.createTestSetWithTests(
        UserExamStepAttemptTestSet(-1, attempt.id, userExam.id, attempt.examStepConfId, testSetConf.id)
      )

    newTestSet
  }

  //===============================================================
  //                      Task - flow
  //===============================================================

  private def createTaskFlowForAttempt(userExam: UserExam,
                                       attempt: UserExamStepAttempt,
                                       variantConf: ExamStepVariantConf): UserExamStepAttemptTaskFlow = {
    val (newTaskFlow, newTaskFlowSteps) =
      taskFlowExamService
        .createTaskFlowWithSteps(attempt.id, userExam.id, attempt.examStepConfId, variantConf.dataSetConfId)

    newTaskFlow
  }

  def verifyTaskFlowStepAnswer(userExamId: Long,
                               stepSequence: Int,
                               stepAttemptId: Long,
                               taskFlowId: Long,
                               taskFlowStepId: Long,
                               answer: String): Option[VerifiedTaskFlowStepAnswer] = {
    val userExam = userExams.find(_.id == userExamId).getOrElse(
      throw new RuntimeException(s"Exam with id: $userExamId not found.")
    )
    val examStepConf = examStepConfs.find(esc => esc.examConfId == userExam.examConfId && esc.sequence == stepSequence).getOrElse(
      throw new RuntimeException(s"Exam ($userExamId) step with sequence: $stepSequence not found.")
    )
    val allStepAttempts = getUserExamStepAttempts(userExam.userId, examStepConf.id)
    val currentStepAttempt = allStepAttempts.find(_.id == stepAttemptId).getOrElse(
      throw new IllegalArgumentException(s"Step attempt with id: $stepAttemptId not found!")
    )

    taskFlowExamService.verifyTaskFlowStepAnswer(taskFlowStepId, answer).map { va =>
      updateAttemptData(userExam, examStepConf, currentStepAttempt, allStepAttempts, va.mistakesAmount, va.isCorrectAnswer)
      va
    }
  }

  def getCurrentTaskFlowStepDto(taskFlowId: Long): Option[TaskFlowStepDto] = {
    taskFlowExamService.getCurrentTaskFlowStep(taskFlowId)
  }

}

object ExamQueries {

}