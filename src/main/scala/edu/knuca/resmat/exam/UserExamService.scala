package edu.knuca.resmat.exam

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.GeneralHelpers
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.ExamStatus.ExamStatus
import edu.knuca.resmat.exam.ExamStepType.ExamStepType
import edu.knuca.resmat.exam.taskflow.{TaskFlowExamService, TaskFlowStepDto, VerifiedTaskFlowStepAnswer}
import edu.knuca.resmat.exam.testset.{TestAnswerDto, TestSetExamService, VerifiedTestAnswerDto}
import edu.knuca.resmat.http.{ResourceLocked, NotAuthorized}
import edu.knuca.resmat.user.{AuthenticatedUser, UsersService}
import org.joda.time.DateTime

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import io.circe.syntax._
import io.circe.generic.auto._

import scala.util.Try

trait StepDataDto

case class UserExamDto(userExam: UserExam, currentStepPreview: Option[UserExamStepPreviewDto], examConf: ExamConf)
case class UserExamStepPreviewDto(sequence: Int, stepType: ExamStepType, description: String)
case class UserExamStepInfoDto(stepConf: ExamStepConf, attempts: Seq[UserExamStepAttempt])
case class UserExamStepAttemptDto(stepConf: ExamStepConf, attempt: UserExamStepAttempt, stepData: StepDataDto)

case class NI(data: String = "not implemented") extends StepDataDto

class UserExamService(val db: DatabaseService)
                     (examService: ExamService,
                      usersService: UsersService,
                      testSetExamService: TestSetExamService,
                      taskFlowExamService: TaskFlowExamService)
                     (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.exam.{UserExamQueries => Q}

  def createUserExam(userId: Long, examConfId: Long): UserExamDto = db.run{ implicit c =>
    val examConfDto = examService.getExamConfDto(examConfId)
    val firstStepConf = examConfDto.stepConfs.sortBy(_.sequence).head
    val ue = UserExam(-1, userId, examConfId, firstStepConf.id, ExamStatus.Initial, None, None, None)
    mapToDto(createUserExam(ue))
  }

  def createUserExam(userExam: UserExam): UserExam = db.run{ implicit c =>
    val insertedIdOpt: Option[Long] = Q.createUserExam(userExam).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert $userExam")
    )
    getUserExam(insertedId)
  }

  def unlockUserExam(id: Long): UserExamDto = db.run{ implicit c =>
    val ue = getUserExam(id)
    mapToDto(updateUserExam(ue.copy(lockedUntil = None)))
  }

  def lockUserExam(id: Long, hoursAmount: Int): UserExamDto = db.run{ implicit c =>
    val ue = getUserExam(id)
    val updatedLockedUntil = ue.lockedUntil.fold(DateTime.now().plusHours(hoursAmount)){ lu =>
      val newValue = DateTime.now().plusHours(hoursAmount)
      if(lu.getMillis > newValue.getMillis) lu else newValue
    }
    mapToDto(
      updateUserExam(
        ue.copy(lockedUntil = Some(updatedLockedUntil))
      )
    )
  }

  def unlockAllForGroup(groupId: Long) = {
    usersService.getStudentsByGroup(groupId).flatMap{ students =>
      val exams = students.flatMap(s => findUserExamsAvailableForUser(s.id.get))
      exams.foreach( e => unlockUserExam(e.userExam.id))
      Future()
    }
  }

  def lockAllForGroup(groupId: Long, hoursAmount: Int) = {
    usersService.getStudentsByGroup(groupId).flatMap{ students =>
      val exams = students.flatMap(s => findUserExamsAvailableForUser(s.id.get))
      exams.foreach( e => lockUserExam(e.userExam.id, hoursAmount))
      Future()
    }
  }

  def getUserExam(id: Long): UserExam = db.run{ implicit c =>
    Q.getUserExam(id).as(Q.ueParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Exam with id: $id not found.")
    )
  }

  def startAndGetUserExamDto(id: Long)(implicit user: AuthenticatedUser): UserExamDto = {
    var userExam = getUserExam(id)
    if(userExam.userId != user.id) {
      throw NotAuthorized()
    }
    if(userExam.status == ExamStatus.Failed) {
      throw new IllegalStateException("Already failed")
    }
    val isLocked = userExam.lockedUntil.exists(lu => lu.getMillis > DateTime.now.getMillis)
    if(isLocked) throw ResourceLocked(userExam.lockedUntil.get)
    if(userExam.status == ExamStatus.Initial) {
      userExam = updateUserExam(userExam.copy(status = ExamStatus.InProgress))
    }
    mapToDto(userExam)
  }

  def getUserExamDto(id: Long)(implicit user: AuthenticatedUser): UserExamDto = {
    val exam = getUserExam(id)
    if(exam.userId != user.id) {
      throw NotAuthorized()
    }
    mapToDto(exam)
  }

  def findUserExamsAvailableForUser(userId: Long): Seq[UserExamDto] = db.run{ implicit c =>
    val ues = Q.findUserExamsByUserId(userId).as(Q.ueParser.*)
    ues.map(mapToDto)
  }

  def getCurrentUserExam(userId: Long): Option[UserExamDto] = db.run{ implicit c =>
    val currentInProgress = Q.findUserExamsInProgress(userId).as(Q.ueParser.singleOpt)
    currentInProgress.fold{
      val userExams = Q.findUserExamsByUserId(userId).as(Q.ueParser.*)
      userExams.find(_.status == ExamStatus.Initial).map(mapToDto)
    }{ ue =>
      Some(mapToDto(ue))
    }
  }

  def updateUserExam(userExam: UserExam): UserExam = db.run{ implicit c =>
    val affected = Q.updateUserExam(userExam).executeUpdate()
    if(affected != 1) {
      throw new RuntimeException(s"Failed to update $userExam")
    }
    Q.getUserExam(userExam.id).as(Q.ueParser.single)
  }

  def deleteUserExam(id: Long) = db.run{ implicit c =>
    val affected = Q.deleteUserExam(id).executeUpdate()
    if(affected != 1) {
      throw new RuntimeException(s"Failed to delete user exam with id $id")
    }
  }

  def findUserExamStepAttempts(userExamId: Long): Seq[UserExamStepAttempt] = db.run{ implicit c =>
    Q.findUserExamStepAttempts(userExamId).as(Q.uesaParser.*)
  }

  def findUserExamStepAttempts(userExamId: Long, examStepConfId: Long): Seq[UserExamStepAttempt] = db.run{ implicit c =>
    Q.findUserExamStepAttempts(userExamId, examStepConfId).as(Q.uesaParser.*)
  }

  private def mapToDto(ue: UserExam): UserExamDto = {
    val configuration = examService.getExamConf(ue.examConfId)
    val currentStep = examService.getExamStepConf(ue.currentStepConfId)
    val currentStepOpt = Some(UserExamStepPreviewDto(currentStep.sequence, currentStep.stepType, currentStep.name)) //todo figure out why Option should be here
    UserExamDto(ue, currentStepOpt, configuration)
  }

  def updateStepAttempt(stepAttempt: UserExamStepAttempt): UserExamStepAttempt = db.run{ implicit c =>
    val affectedRows = Q.updateUserExamStepAttempt(stepAttempt).executeUpdate()
    if(affectedRows != 1) {
      throw new RuntimeException("Failed to update UserExamStepAttempt")
    }
    Q.getUserExamStepAttempt(stepAttempt.id).as(Q.uesaParser.single)
  }

  private def createStepAttempt(attempt: UserExamStepAttempt): UserExamStepAttempt = db.run{ implicit c =>
    val existingAttempts = findUserExamStepAttempts(attempt.userExamId).filter(_.examStepConfId == attempt.examStepConfId)
    val updatedAttemptNumber = if(existingAttempts.nonEmpty) {
      existingAttempts.maxBy(_.attemptNumber).attemptNumber + 1
    } else {
      1
    }
    val updatedAttempt = attempt.copy(attemptNumber = updatedAttemptNumber)
    val insertedIdOpt: Option[Long] = Q.createUserExamStepAttempt(updatedAttempt).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert $attempt")
    )
    Q.getUserExamStepAttempt(insertedId).as(Q.uesaParser.single)
  }

  def getUserExamResultByUserExamId(userExamId: Long): UserExamResult = db.run{ implicit c =>
    Q.getUserExamResultByUserExamId(userExamId).as(Q.uerParser.singleOpt).getOrElse(
      throw new RuntimeException(s"User exam result with id: $userExamId not found.")
    )
  }

  def findUserExamResults(userId: Long): Seq[UserExamResult] = db.run{ implicit c =>
    Q.findUserExamResults(userId).as(Q.uerParser.*)
  }

  def getOrCreateUserExamResult(userExamId: Long): UserExamResult = {
    Try(getUserExamResultByUserExamId(userExamId)).getOrElse {
      val result = calculateUserExamResult(userExamId)
      createUserExamResult(result)
    }
  }

  def createUserExamResult(userExamResult: UserExamResult): UserExamResult = db.run{ implicit c =>
    val insertedIdOpt: Option[Long] = Q.createUserExamResult(userExamResult).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to insert $userExamResult")
    )
    Q.getUserExamResult(insertedId).as(Q.uerParser.single)
  }

  //===============================================================
  //                      User - exam - step
  //===============================================================

  def getUserExamCurrentStepWithAttemptData(userExamId: Long): Option[UserExamStepAttemptDto] = {
    val userExam = getUserExam(userExamId)
    val examStepConf = examService.getExamStepConf(userExam.currentStepConfId)
    getUserExamStepCurrentAttempt(userExam.id, examStepConf.sequence)
  }

  def getUserExamStepInfos(userExamId: Long): Seq[UserExamStepInfoDto] = {
    val userExam = getUserExam(userExamId)
    val stepConfs = examService.findExamStepConfsByExamConfId(userExam.examConfId)
    stepConfs.map(getStepInfo(userExam.id, _))
  }

  def getUserExamStepInfo(userExamId: Long, sequence: Int): Option[UserExamStepInfoDto] = {
    val steps = getUserExamStepInfos(userExamId)
    steps.find(_.stepConf.sequence == sequence)
  }

  //todo make this method return uncompleted parts
  def submitStep(userExamId: Long, stepSequence: Int): Boolean = {
    val userExam = getUserExam(userExamId)
    val stepConfs = examService.findExamStepConfsByExamConfId(userExam.examConfId)
    val examStepConf = stepConfs.find(_.sequence == stepSequence).getOrElse(
      throw new RuntimeException(s"Exam ($userExamId) step with sequence: $stepSequence not found.")
    )

    val previousNotSubmittedSteps = getPreviousNotSubmittedSteps(stepConfs, stepSequence, userExam.id)
    if(previousNotSubmittedSteps.nonEmpty) {
      throw new IllegalStateException(s"Failed to submit user exam id: [$userExamId] step with sequence: [$stepSequence]. " +
        s"Next previous steps are not submitted yet: $previousNotSubmittedSteps")
    }

    val allStepAttempts = findUserExamStepAttempts(userExam.id, examStepConf.id)

    val notSubmittedAttempts = allStepAttempts.filter(_.status == ExamStepStatus.NotSubmitted)
    val successAttempts = allStepAttempts.filter(_.status == ExamStepStatus.Success)

    if(successAttempts.nonEmpty || notSubmittedAttempts.size != 1) {
      throw new IllegalStateException(s"When submitting user exam id: [$userExamId] step sequence: [$stepSequence] " +
        s"success attempts: $successAttempts, not submitted attempts: $notSubmittedAttempts")
    }

    val stepAttempt = notSubmittedAttempts.head
    if(isStepCompleted(stepAttempt, examStepConf)) {
      updateStepAttempt(stepAttempt.copy(status = ExamStepStatus.Success))
      //Change exam's current step to next or mark exam as Successful if no more steps
      if(isLastStepToBeSubmitted(stepConfs, stepSequence)) {
        val updatedUserExam = updateUserExam(userExam.copy(status = ExamStatus.Success, finished = Some(DateTime.now)))
        stepConfs.find(_.sequence == stepSequence + 1).foreach( nextStepConf =>
          updateUserExam(updatedUserExam.copy(currentStepConfId = nextStepConf.id))
        )
      } else {
        val nextStepConf = stepConfs.find(_.sequence == stepSequence + 1).getOrElse(
          throw new IllegalStateException(s"Next after sequence: $stepSequence step not found: $stepConfs")
        )
        updateUserExam(userExam.copy(currentStepConfId = nextStepConf.id))
      }
      true
    } else {
      false
    }
  }

  def getPreviousNotSubmittedSteps(stepConfs: Seq[ExamStepConf], stepSequence: Int, userExamId: Long): Seq[ExamStepConf] = {
    val previousSteps = stepConfs.filter(_.sequence < stepSequence)
    previousSteps.filter{ stepConf =>
      !findUserExamStepAttempts(userExamId, stepConf.id).exists(_.status == ExamStepStatus.Success)
    }
  }

  def isStepCompleted(stepAttempt: UserExamStepAttempt, examStepConf: ExamStepConf): Boolean = {
    examStepConf.stepType match {
      case ExamStepType.TestSet =>
        val stepAttemptTestSet = testSetExamService.getUserExamTestSetByAttemptId(stepAttempt.id)
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

  private def isLastStepToBeSubmitted(stepConfs: Seq[ExamStepConf], stepSequence: Int): Boolean = {
    if(stepConfs.isEmpty) {
      throw new IllegalStateException(s"Empty step conf list passed to check ${stepSequence} is last step")
    }
    stepConfs.filter(_.hasToBeSubmitted).sortBy(_.sequence).last.sequence == stepSequence
  }

  private def getStepInfo(userExamId: Long, stepConf: ExamStepConf): UserExamStepInfoDto = {
    val stepAttempts = findUserExamStepAttempts(userExamId, stepConf.id)
    UserExamStepInfoDto(stepConf, stepAttempts)
  }

  //===============================================================
  //                      User - exam - step  - attempt
  //===============================================================

  def getUserExamStepAttempts(userExamId: Long, sequence: Int): Seq[UserExamStepAttemptDto] = {
    val userExam = getUserExam(userExamId)
    val examStepConf = examService.getExamStepConfByExamConfIdAndSequence(userExam.examConfId, sequence)
    val attempts = findUserExamStepAttempts(userExam.id, examStepConf.id)
    attempts.flatMap(getAttemptDto(_, examStepConf))
  }

  def getUserExamStepCurrentAttempt(userExamId: Long, stepSequence: Int): Option[UserExamStepAttemptDto] = {
    val userExam = getUserExam(userExamId)
    val examStepConf = examService.getExamStepConfByExamConfIdAndSequence(userExam.examConfId, stepSequence)
    val existingAttempts = findUserExamStepAttempts(userExam.id, examStepConf.id)

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
      } else { //Lock exam for 24 hrs
        updateUserExam(userExam.copy(lockedUntil = Some(DateTime.now.plusHours(24))))
      }
    } else if(updatedMistakesAmount > currentStepAttempt.mistakesAmount){ //If any mistakes - update attempt
      updateStepAttempt(currentStepAttempt.copy(mistakesAmount = updatedMistakesAmount))
    }
  }

  private def createAttempt(userExam: UserExam, examStepConf: ExamStepConf): UserExamStepAttempt = {
    val newAttempt =
      createStepAttempt(
        UserExamStepAttempt(-1, userExam.id, examStepConf.id, 0, -1, ExamStepStatus.NotSubmitted)
      )

    //todo handle case when data set is not created - rollback attempt creation
    examStepConf.stepType match {
      case ExamStepType.TestSet =>
        val dataSet = examStepConf.dataSet.asInstanceOf[ExamStepTestSetDataSet]
        val testSetConf = testSetExamService.getTestSetConf(dataSet.testSetConfId)
        createNewTestSetForAttempt(newAttempt, testSetConf)
      case ExamStepType.TaskFlow =>
        val dataSet = examStepConf.dataSet.asInstanceOf[ExamStepTaskFlowDataSet]
        createTaskFlowForAttempt(newAttempt, userExam.id, userExam.examConfId, dataSet.taskFlowConfId, dataSet.problemConfId)
      case ExamStepType.Results => ()
      case anyOther =>
        throw new IllegalArgumentException(s"Unhandled ExamStepDataType: $anyOther")
    }

    newAttempt
  }

  private def getAttemptDto(attempt: UserExamStepAttempt, examStepConf: ExamStepConf): Option[UserExamStepAttemptDto] = {
    examStepConf.stepType match {
      case ExamStepType.TestSet =>
        val testSetDto = testSetExamService.getTestSetDto(attempt.id)
        Some(UserExamStepAttemptDto(examStepConf, attempt, testSetDto))
      case ExamStepType.TaskFlow =>
        val taskFlowDto = taskFlowExamService.getTaskFlowDto(attempt.id).getOrElse(
          throw new RuntimeException(s"Task flow data not found. Attempt: $attempt")
        )
        Some(UserExamStepAttemptDto(examStepConf, attempt, taskFlowDto))
      case ExamStepType.Results =>
        val userExamResult = getOrCreateUserExamResult(attempt.userExamId)
        Some(UserExamStepAttemptDto(examStepConf, attempt, userExamResult))
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
                          submittedOptions: Seq[Long]): VerifiedTestAnswerDto = {
    val userExam = getUserExam(userExamId)
    val examStepConf = examService.getExamStepConfByExamConfIdAndSequence(userExam.examConfId, stepSequence)
    val allStepAttempts = findUserExamStepAttempts(userExam.id, examStepConf.id)
    val currentStepAttempt = allStepAttempts.find(_.id == stepAttemptId).getOrElse(
      throw new IllegalArgumentException(s"Step attempt with id: $stepAttemptId not found!")
    )
    val stepAttemptTestSet = testSetExamService.getUserExamTestSetByAttemptId(currentStepAttempt.id)

    val va = testSetExamService.verifyTestSetTestAnswer(stepAttemptTestSet.id, TestAnswerDto(testId, submittedOptions))
    updateAttemptData(userExam, examStepConf, currentStepAttempt, allStepAttempts, va.mistakesAmount, va.isCorrectAnswer)
    va
  }

  private def createNewTestSetForAttempt(attempt: UserExamStepAttempt,
                                         testSetConf: TestSetConf): UserExamStepAttemptTestSet = {
    val (newTestSet, newTestSetTests) =
      testSetExamService.createTestSetWithTests(
        UserExamStepAttemptTestSet(-1, attempt.id, testSetConf.id),
        testSetConf.maxTestsAmount
      )

    newTestSet
  }

  //===============================================================
  //                      Task - flow
  //===============================================================

  private def createTaskFlowForAttempt(attempt: UserExamStepAttempt,
                                       userExamId: Long,
                                       examConfId: Long,
                                       taskFlowConfId: Long,
                                       problemConfId: Long): UserExamStepAttemptTaskFlow = {
    val (newTaskFlow, newTaskFlowSteps) =
      taskFlowExamService
        .createTaskFlowWithSteps(attempt.id, userExamId, examConfId, attempt.examStepConfId, taskFlowConfId, problemConfId)

    newTaskFlow
  }

  def verifyTaskFlowStepAnswer(userExamId: Long,
                               stepSequence: Int,
                               stepAttemptId: Long,
                               taskFlowId: Long,
                               taskFlowStepId: Long,
                               answer: String): Option[VerifiedTaskFlowStepAnswer] = {
    val userExam = getUserExam(userExamId)
    val examStepConf = examService.getExamStepConfByExamConfIdAndSequence(userExam.examConfId, stepSequence)
    val allStepAttempts = findUserExamStepAttempts(userExam.id, examStepConf.id)
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

  //===============================================================
  //                      Results
  //===============================================================

  def calculateUserExamResult(userExamId: Long): UserExamResult = {
    val userExam = getUserExam(userExamId)

    if(!Seq(ExamStatus.Success, ExamStatus.Failed).contains(userExam.status)) {
      throw new IllegalStateException(s"Failed to compose exam result! Reason: Exam status is: ${userExam.status}")
    }

    val examConf = examService.getExamConf(userExam.examConfId)
    val user = Await.result(usersService.getById(userExam.userId), 5 seconds).getOrElse(
      throw new RuntimeException(s"User with id: ${userExam.userId} not found!")
    )
    val studentGroup = user.studentGroupId.map { studentGroupId =>
      Await.result(usersService.getStudentGroupById(studentGroupId), 5 seconds).getOrElse(
        throw new RuntimeException(s"Student group with id: $studentGroupId not found!")
      )
    }

    val duration = calculateDuration(userExam)

    val stepResults = calculateStepResults(userExam, examConf)

    val score = calculateScore(examConf, stepResults)

    UserExamResult(
      -1,
      userExam.id,
      examConf.id,
      userExam.userId,
      examConf.name,
      user.firstName + " " + user.lastName,
      studentGroup.map(_.name),
      duration,
      stepResults.map(_._1),
      score,
      examConf.maxScore
    )
  }

  def calculateScore(examConf: ExamConf, stepResults: Seq[(UserExamStepResult, ExamStepConf)]): Int = {

    def calculateReduceForStep(tuple: (UserExamStepResult, ExamStepConf)): Int = {
      val res: UserExamStepResult = tuple._1
      val conf: ExamStepConf = tuple._2
      val overAttempts = res.attemptsAmount - 1
      val reduceMaxScoreOverAttempts: Double = (overAttempts * conf.attemptValuePercents) / 100.0
      val reduceMaxScoreMistakesAmount: Double = (res.mistakesAmount * conf.mistakeValuePercents) / 100.0
      (Math.round(conf.maxScore * reduceMaxScoreOverAttempts) + Math.round(conf.maxScore * reduceMaxScoreMistakesAmount)).toInt
    }

    examConf.maxScore - stepResults.map(calculateReduceForStep).sum
  }

  def calculateStepResults(userExam: UserExam, examConf: ExamConf): Seq[(UserExamStepResult, ExamStepConf)] = {
    val submittableStepConfs = examService.findSubmittableExamStepConf(userExam.examConfId)
    val stepAttempts = findUserExamStepAttempts(userExam.id)
    submittableStepConfs.map { stepConf =>
      val currentStepConfAttempts = stepAttempts.filter(_.examStepConfId == stepConf.id)
      if(currentStepConfAttempts.size < 1) {
        throw new IllegalStateException(s"$stepConf has no attempts for user exam with id: ${userExam.id}")
      }
      val mistakesAmount = currentStepConfAttempts.map(_.mistakesAmount).sum
      (
        UserExamStepResult(stepConf.id, stepConf.sequence, stepConf.name, currentStepConfAttempts.size, mistakesAmount, -1),
        stepConf
      )
    }
  }

  def calculateDuration(userExam: UserExam): Long = {
    val started: Long = userExam.started.map(_.getMillis).getOrElse(0)
    val finished: Long = userExam.finished.map(_.getMillis).getOrElse(0)
    if(started == 0 || finished == 0) {
      throw new IllegalStateException(s"UserExams duration is not set properly - started: ${userExam.started}, finished: ${userExam.finished}")
    }
    finished - started
  }

}

object UserExamQueries {

  import io.circe.parser._

  import anorm.SqlParser.{int, long, str, bool, date}

  object UE {
    val table = "user_exams"
    val id = "id"
    val userId = "user_id"
    val examConfId = "exam_conf_id"
    val currentStepConfId = "current_step_conf_id"
    val status = "status"
    val lockedUntil = "locked_until"
    val started = "started"
    val finished = "finished"
  }

  object UESA {
    val table = "user_exam_step_attempts"
    val id = "id"
    val userExamId = "user_exam_id"
    val examStepConfId = "exam_step_conf_id"
    val mistakesAmount = "mistakes_amount"
    val attemptNumber = "attempt_number"
    val status = "status"
  }

  object UER {
    val table = "user_exam_results"
    val id = "id"
    val userExamId = "user_exam_id"
    val examConfId = "exam_conf_id"
    val userId = "user_id"
    val examName = "exam_name"
    val studentName = "student_name"
    val studentGroupName = "student_group_name"
    val durationMillis = "duration_millis"
    val stepResults = "step_results"
    val score = "score"
    val maxScore = "max_score"
  }

  val ueParser = for {
    id <- long(UE.id)
    userId <- long(UE.userId)
    examConfId <- long(UE.examConfId)
    currentStepConfId <- long(UE.currentStepConfId)
    status <- int(UE.status)
    lockedUntil <- date(UE.lockedUntil).?
    started <- date(UE.started).?
    finished <- date(UE.finished).?
  } yield UserExam(id, userId, examConfId, currentStepConfId, ExamStatus(status),
    lockedUntil.map(new DateTime(_)), started.map(new DateTime(_)), finished.map(new DateTime(_)))

  val uesaParser = for {
    id <- long(UESA.id)
    userExamId <- long(UESA.userExamId)
    examStepConfId <- long(UESA.examStepConfId)
    mistakesAmount <- int(UESA.mistakesAmount)
    attemptNumber <- int(UESA.attemptNumber)
    status <- int(UESA.status)
  } yield UserExamStepAttempt(id, userExamId, examStepConfId, mistakesAmount, attemptNumber, ExamStepStatus(status))

  val uerParser = for {
    id <- long(UER.id)
    userExamId <- long(UER.userExamId)
    examConfId <- long(UER.examConfId)
    userId <- long(UER.userId)
    examName <- str(UER.examName)
    studentName <- str(UER.studentName)
    studentGroupName <- str(UER.studentGroupName).?
    durationMillis <- long(UER.durationMillis)
    stepResults <- str(UER.stepResults)
    score <- int(UER.score)
    maxScore <- int(UER.maxScore)
  } yield UserExamResult(
    id,
    userExamId,
    examConfId,
    userId,
    examName,
    studentName,
    studentGroupName,
    durationMillis,
    parseStepResults(stepResults),
    score,
    maxScore
  )

  def createUserExam(ue: UserExam) =
    SQL(
      s"""INSERT INTO ${UE.table} (
         |${UE.userId},
         |${UE.examConfId},
         |${UE.currentStepConfId},
         |${UE.status},
         |${UE.started},
         |${UE.finished}
         |)
         |VALUES (
         |{userId},
         |{examConfId},
         |{currentStepConfId},
         |{status},
         |{started},
         |{finished}
         |)
       """.stripMargin)
      .on("userId" -> ue.userId)
      .on("examConfId" -> ue.examConfId)
      .on("currentStepConfId" -> ue.currentStepConfId)
      .on("status" -> ue.status.id)
      .on("started" -> ue.started.map(GeneralHelpers.toMysql).orNull)
      .on("finished" -> ue.finished.map(GeneralHelpers.toMysql).orNull)

  def updateUserExam(ue: UserExam) =
    SQL(
      s"""UPDATE ${UE.table} SET
         |${UE.currentStepConfId} = {currentStepConfId},
         |${UE.status} = {status},
         |${UE.lockedUntil} = {lockedUntil},
         |${UE.finished} = {finished}
         |WHERE ${UE.id} = {id}""".stripMargin)
      .on("id" -> ue.id)
      .on("currentStepConfId" -> ue.currentStepConfId)
      .on("status" -> ue.status.id)
      .on("lockedUntil" -> ue.lockedUntil.map(GeneralHelpers.toMysql).orNull)
      .on("finished" -> ue.finished.map(GeneralHelpers.toMysql).orNull)

  def createUserExamStepAttempt(uesa: UserExamStepAttempt) =
    SQL(
      s"""INSERT INTO ${UESA.table} (
         |${UESA.userExamId},
         |${UESA.examStepConfId},
         |${UESA.mistakesAmount},
         |${UESA.attemptNumber},
         |${UESA.status}
         |)
         |VALUES (
         |{userExamId},
         |{examStepConfId},
         |{mistakesAmount},
         |{attemptNumber},
         |{status}
         |)
       """.stripMargin)
      .on("userExamId" -> uesa.userExamId)
      .on("examStepConfId" -> uesa.examStepConfId)
      .on("mistakesAmount" -> uesa.mistakesAmount)
      .on("attemptNumber" -> uesa.attemptNumber)
      .on("status" -> uesa.status.id)

  def updateUserExamStepAttempt(uesa: UserExamStepAttempt) =
    SQL(
      s"""UPDATE ${UESA.table} SET
         |${UESA.mistakesAmount}={mistakesAmount},
         |${UESA.attemptNumber}={attemptNumber},
         |${UESA.status}={status}
         |WHERE id = {id}
       """.stripMargin)
      .on("id" -> uesa.id)
      .on("mistakesAmount" -> uesa.mistakesAmount)
      .on("attemptNumber" -> uesa.attemptNumber)
      .on("status" -> uesa.status.id)

  def createUserExamResult(uer: UserExamResult) =
    SQL(
      s"""INSERT INTO ${UER.table} (
         |${UER.userExamId},
         |${UER.examConfId},
         |${UER.userId},
         |${UER.examName},
         |${UER.studentName},
         |${UER.studentGroupName},
         |${UER.durationMillis},
         |${UER.stepResults},
         |${UER.score},
         |${UER.maxScore}
         |)
         |VALUES (
         |{userExamId},
         |{examConfId},
         |{userId},
         |{examName},
         |{studentName},
         |{studentGroupName},
         |{durationMillis},
         |{stepResults},
         |{score},
         |{maxScore}
         |)
         """.stripMargin)
      .on("userExamId" -> uer.userExamId)
      .on("examConfId" -> uer.examConfId)
      .on("userId" -> uer.userId)
      .on("examName" -> uer.examName)
      .on("studentName" -> uer.studentName)
      .on("studentGroupName" -> uer.studentGroupName.orNull)
      .on("durationMillis" -> uer.durationMillis)
      .on("stepResults" -> uer.stepResults.asJson.toString)
      .on("score" -> uer.score)
      .on("maxScore" -> uer.maxScore)

  def getUserExam(id: Long) = SQL(s"SELECT * FROM ${UE.table} WHERE ${UE.id} = {id}").on("id" -> id)

  def findUserExamsByUserId(userId: Long) =
    SQL(s"SELECT * FROM ${UE.table} WHERE ${UE.userId} = {userId}").on("userId" -> userId)

  def findUserExamsInProgress(userId: Long) =
    SQL(s"SELECT * FROM ${UE.table} " +
      s"WHERE ${UE.userId} = {userId} AND ${UE.status} = ${ExamStatus.InProgress.id}").on("userId" -> userId)

  def getUserExamResult(id: Long) =
    SQL(s"SELECT * FROM ${UER.table} WHERE ${UER.id} = {id}").on("id" -> id)

  def findUserExamResults(userId: Long) =
    SQL(s"SELECT * FROM ${UER.table} WHERE ${UER.userId} = {userId}").on("userId" -> userId)

  def getUserExamResultByUserExamId(userExamId: Long) =
    SQL(s"SELECT * FROM ${UER.table} WHERE ${UER.userExamId} = {userExamId}").on("userExamId" -> userExamId)

  def getUserExamStepAttempt(id: Long) =
    SQL(s"SELECT * FROM ${UESA.table} WHERE ${UESA.id} = {id}").on("id" -> id)

  def findUserExamStepAttempts(userExamId: Long) =
    SQL(s"SELECT * FROM ${UESA.table} WHERE ${UESA.userExamId} = {userExamId}").on("userExamId" -> userExamId)

  def findUserExamStepAttempts(userExamId: Long, examStepConfId: Long) =
    SQL(s"SELECT * FROM ${UESA.table} WHERE ${UESA.userExamId} = {userExamId} AND ${UESA.examStepConfId} = {examStepConfId}")
      .on("userExamId" -> userExamId)
      .on("examStepConfId" -> examStepConfId)

  def deleteUserExam(id: Long) =
    SQL(s"DELETE FROM ${UE.table} WHERE ${UE.id} = {id}").on("id" -> id)

  def deleteUserExamStepAttempts(ids: Seq[Long]) =
    SQL(s"DELETE FROM ${UESA.table} WHERE ${UESA.id} IN ({ids})").on("ids" -> ids)


  private def parseStepResults(json: String): Seq[UserExamStepResult] = {
    decode[Seq[UserExamStepResult]](json).fold( e =>
      throw new RuntimeException(s"Failed to parse Seq[UserExamStepResult] in json: $json", e),
      r => r
    )
  }

}