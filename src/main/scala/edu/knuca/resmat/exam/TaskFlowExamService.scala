package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

case class TaskFlowDto(taskFlowConf: TaskFlowConf,
                       problemConf: ProblemConf,
                       problemVariantConf: ProblemVariantConf,
                       taskFlow: UserExamStepAttemptTaskFlow) extends StepDataDto
case class TaskFlowStepDto(taskFlowStepConf: TaskFlowStepConf,
                           stepAttemptTaskFlowStep: UserExamStepAttemptTaskFlowStep,
                           taskFlowStepData: String)

case class InputSetAnswerDto(inputSetId: Long, inputAnswers: Seq[InputSetInputAnswer])
case class InputSetInputAnswer(id: Int, value: Option[Double] = None)

case class VerifiedTaskFlowStepAnswer(isCorrectAnswer: Boolean, mistakesAmount: Int, answer: String)

case class VerifiedInputSetAnswer(inputSetId: Long, isCorrectAnswer: Boolean, mistakesAmount: Int, answer: Map[Int, Boolean])

class TaskFlowExamService(val db: DatabaseService)
                         (testSetExamService: TestSetExamService)
                         (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.http.JsonProtocol._

  val problemConfs: List[ProblemConf] = List(
    ProblemConf(1, "Кільцева пластина")
  )
  val problemVariantConfs: List[ProblemVariantConf] = List(
    ProblemVariantConf(1, 1, "Дані по варіанту")
  )
  val calculatedProblemVariantConfs: List[CalculatedProblemVariantConf] = List(
    CalculatedProblemVariantConf(1, 1, "Обраховані дані по варіанту для перевірки студента")
  )

  val taskFlowConfs: List[TaskFlowConf] = List(
    TaskFlowConf(1, 1)
  )
  val taskFlowStepConfs: List[TaskFlowStepConf] = List(
    TaskFlowStepConf(1, 1, "Визначення типу пластини", 1, TaskFlowStepType.Test, TaskFlowTest(999).asJson.toString()),
    TaskFlowStepConf(2, 1, "Введіть значення граничних умов, якщо умова невідома - залиште поле пустим", 2, 
      TaskFlowStepType.InputSet, InputSet(1, "InputSetName", Seq(
        InputSetInput(1, "w(a)", "На внутрішньому контурі", "м"),
        InputSetInput(2, "{phi}{(a)}", "На внутрішньому контурі", "рад"),
        InputSetInput(3, "Mr(a)", "На внутрішньому контурі", "кНм/м"),
        InputSetInput(4, "Qr(a)", "На внутрішньому контурі", "кН/м"),
        InputSetInput(5, "w(b)", "На зовнішньому контурі", "м"),
        InputSetInput(6, "{phi}{(b)}", "На зовнішньому контурі", "рад"),
        InputSetInput(7, "Mr(b)", "На зовнішньому контурі", "кНм/м"),
        InputSetInput(8, "Qr(b)", "На зовнішньому контурі", "кН/м")
      ), InputSetAnswerDto(1, Seq(
        InputSetInputAnswer(1),
        InputSetInputAnswer(2),
        InputSetInputAnswer(3),
        InputSetInputAnswer(4),
        InputSetInputAnswer(5),
        InputSetInputAnswer(6),
        InputSetInputAnswer(7),
        InputSetInputAnswer(8)
      ))).asJson.toString()
    ),
    TaskFlowStepConf(3, 1, "Чи забезпечується міцність перерізу?", 3, TaskFlowStepType.Test, TaskFlowTest(1000).asJson.toString())
  )

  val taskFlowConfProblemVariantConfs: List[TaskFlowConfProblemVariantConf] = List(
    TaskFlowConfProblemVariantConf(1, 1, 1)
  )

  //===============================================================
  //                      User specific data
  //===============================================================

  val stepAttemptTaskFlows: ListBuffer[UserExamStepAttemptTaskFlow] = ListBuffer()
  val stepAttemptTaskFlowSteps: ListBuffer[UserExamStepAttemptTaskFlowStep] = ListBuffer()

  //===============================================================
  //                      Code
  //===============================================================

  def getTaskFlowDto(stepAttemptId: Long): Option[TaskFlowDto] = {
    val taskFlow = stepAttemptTaskFlows.find(_.stepAttemptId == stepAttemptId).getOrElse(
      throw new IllegalArgumentException(s"Task flow for attempt id: $stepAttemptId not found!")
    )
    val taskFlowConf = taskFlowConfs.find(_.id == taskFlow.taskFlowConfId).getOrElse(
      throw new RuntimeException(s"Task flow conf with id: ${taskFlow.taskFlowConfId} not found! For task flow: $taskFlow")
    )
    val problemVariantConf = problemVariantConfs.find(_.id == taskFlow.problemVariantConfId).getOrElse(
      throw new RuntimeException(s"Problem variant conf with id: ${taskFlow.problemVariantConfId} not found!")
    )
    val problemConf = problemConfs.find(_.id == problemVariantConf.problemConfId).getOrElse(
      throw new RuntimeException(s"Problem conf with id: ${problemVariantConf.problemConfId} not found!")
    )
    Some(TaskFlowDto(taskFlowConf, problemConf, problemVariantConf, taskFlow))
  }

  def getCurrentTaskFlowStep(stepAttemptId: Long): Option[TaskFlowStepDto] = {
    val taskFlow = stepAttemptTaskFlows.find(_.stepAttemptId == stepAttemptId).getOrElse(
      throw new IllegalArgumentException(s"Task flow for attempt id: $stepAttemptId not found!")
    )
    getTaskFlowStep(taskFlow.id, taskFlow.currentStepId)
  }

  def getTaskFlowStep(taskFlowId: Long, taskFlowStepId: Long): Option[TaskFlowStepDto] = {
    val taskFlowStep = stepAttemptTaskFlowSteps.find(_.id == taskFlowStepId).getOrElse(
      throw new RuntimeException(s"Task flow step with id: $taskFlowStepId not found!")
    )
    val taskFlowStepConf = taskFlowStepConfs.find(_.id == taskFlowStep.taskFlowStepConfId).getOrElse(
      throw new RuntimeException(s"Task flow step conf with id not found!")
    )

    val taskFlowStepData: String = taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val taskFlowTest = decode[TaskFlowTest](taskFlowStepConf.stepData).fold(er => None, test => Some(test)).getOrElse(
          throw new RuntimeException(s"Failed to parse test in $taskFlowStepConf")
        )
        val testConf = testSetExamService.getTestConfs(Seq(taskFlowTest.testId)).headOption.getOrElse(
          throw new RuntimeException(s"Test with id: ${taskFlowTest.testId}")
        )
        testConf.asJson.toString()
      case TaskFlowStepType.InputSet =>
        val inputSet = decode[InputSet](taskFlowStepConf.stepData).fold(er => None, is => Some(is)).getOrElse(
          throw new RuntimeException(s"Failed to parse input set in $taskFlowStepConf")
        )
        inputSet.asJson.toString()
      case TaskFlowStepType.Charts =>
        "not implemented"
    }
    Some(TaskFlowStepDto(taskFlowStepConf, taskFlowStep, taskFlowStepData))
  }

  def verifyTaskFlowStepAnswer(taskFlowStepId: Long, answer: String): Option[VerifiedTaskFlowStepAnswer] = {

    val taskFlowStep = stepAttemptTaskFlowSteps.find(s => s.id == taskFlowStepId).getOrElse(
      throw new RuntimeException(s"Task flow step with id: $taskFlowStepId not found!")
    )
    val taskFlowStepConf = taskFlowStepConfs.find(sc => sc.id == taskFlowStep.taskFlowStepConfId).getOrElse(
      throw new RuntimeException(s"Task flow step conf with id: ${taskFlowStep.taskFlowStepConfId} not found!")
    )
    taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val testAnswer = decode[TestAnswerDto](answer).fold(er => None, test => Some(test)).getOrElse(
          throw new RuntimeException(s"Failed to parse test answer in $answer")
        )
        val verifiedAnswer = testSetExamService.verifyTestAnswer(testAnswer)
        verifiedAnswer.map{ va =>
          updateTaskFlowStep(taskFlowStep, va.isCorrectAnswer, va.mistakesAmount)
          VerifiedTaskFlowStepAnswer(va.isCorrectAnswer, va.mistakesAmount, va.asJson.toString())
        }
      case TaskFlowStepType.InputSet =>
        val inputSetAnswer = decode[InputSetAnswerDto](answer).fold(er => None, is => Some(is)).getOrElse(
          throw new RuntimeException(s"Failed to parse data in $taskFlowStepConf")
        )
        val taskFlow = stepAttemptTaskFlows.find(_.id == taskFlowStep.stepAttemptTaskFlowId).getOrElse(
          throw new RuntimeException(s"TaskFlow with id: ${taskFlowStep.stepAttemptTaskFlowId} not found!")
        )
        val inputSet = decode[InputSet](taskFlowStepConf.stepData).fold(er => None, is => Some(is)).getOrElse(
          throw new RuntimeException(s"Failed to parse input set in $taskFlowStepConf")
        )
        val verifiedAnswer = verifyInputSet(taskFlow.problemVariantConfId, inputSetAnswer, inputSet)
        verifiedAnswer.map{ va =>
          updateTaskFlowStep(taskFlowStep, va.isCorrectAnswer, va.mistakesAmount)
          VerifiedTaskFlowStepAnswer(va.isCorrectAnswer, va.mistakesAmount, va.asJson.toString())
        }
      case TaskFlowStepType.Charts =>
        Some(VerifiedTaskFlowStepAnswer(true, 0, "not implemented"))
    }
  }

  def updateTaskFlowStep(taskFlowStep: UserExamStepAttemptTaskFlowStep,
                         isCorrectAnswer: Boolean,
                         mistakesAmount: Int): UserExamStepAttemptTaskFlowStep = {
    if(isCorrectAnswer) {
      updateTaskFlowStep(taskFlowStep.copy(done = true))
    } else {
      updateTaskFlowStep(taskFlowStep.copy(mistakes = taskFlowStep.mistakes + mistakesAmount))
    }
  }

  def updateTaskFlowStep(taskFlowStep: UserExamStepAttemptTaskFlowStep): UserExamStepAttemptTaskFlowStep = {
    val index = stepAttemptTaskFlowSteps.indexWhere(_.id == taskFlowStep.id)
    stepAttemptTaskFlowSteps.update(index, taskFlowStep)
    taskFlowStep
  }

  def verifyInputSet(problemVariantConfId: Long, inputSetAnswer: InputSetAnswerDto, inputSet: InputSet): Option[VerifiedInputSetAnswer] = {
//    calculatedProblemVariantConfs.find(_.problemVariantConfId == problemVariantConfId).map( cpvc =>
      //todo Verify based on calculated data
//    )
    var isCorrectAnswer = true
    var mistakesAmount = 0
    val verifiedAnswers: Map[Int, Boolean] = inputSet.answer.inputAnswers.map{ correctAnswer =>
      inputSetAnswer.inputAnswers.find(_.id == correctAnswer.id) match {
        case Some(inputSetInputAnswer) =>
          if(inputSetInputAnswer.value != correctAnswer.value) {
            isCorrectAnswer = false
            mistakesAmount = mistakesAmount + 1
            (correctAnswer.id, false)
          } else {
            (correctAnswer.id, true)
          }
        case None =>
          isCorrectAnswer = false
          mistakesAmount = mistakesAmount + 1
          (correctAnswer.id, false)
      }
    }.toMap
    Some(VerifiedInputSetAnswer(inputSetAnswer.inputSetId, isCorrectAnswer, mistakesAmount, verifiedAnswers))
  }

  def getNotCompletedTaskFlowSteps(stepAttemptId: Long): Seq[UserExamStepAttemptTaskFlowStep] = {
    val taskFlow = stepAttemptTaskFlows.find(_.stepAttemptId == stepAttemptId).getOrElse(
      throw new IllegalArgumentException(s"Task flow for attempt id: $stepAttemptId not found!")
    )
    stepAttemptTaskFlowSteps.filter(s => s.stepAttemptTaskFlowId == taskFlow.id && !s.done)
  }

  def createTaskFlowWithSteps(stepAttemptId: Long,
                              userExamId: Long,
                              examStepConfId: Long,
                              taskFlowConfProblemVariantConfId: Long): (UserExamStepAttemptTaskFlow, Seq[UserExamStepAttemptTaskFlowStep]) = {
    val taskFlowConfProblemVariantConf = taskFlowConfProblemVariantConfs.find(_.id == taskFlowConfProblemVariantConfId).getOrElse(
      throw new RuntimeException(s"Task flow conf problem variant conf with id: $taskFlowConfProblemVariantConfId not found!")
    )
    val stepConfs = taskFlowStepConfs.filter(_.taskFlowConfId == taskFlowConfProblemVariantConf.taskFlowConfId)

    val taskFlow = createTaskFlow(
      UserExamStepAttemptTaskFlow(
        -1,
        stepAttemptId,
        userExamId,
        examStepConfId,
        taskFlowConfProblemVariantConf.taskFlowConfId,
        taskFlowConfProblemVariantConf.problemVariantConfId,
        1
      )
    )

    val taskFlowSteps = stepConfs.map{ sc =>
      createTaskFlowStep(UserExamStepAttemptTaskFlowStep(-1, taskFlow.id, sc.id))
    }

    (taskFlow, taskFlowSteps)
  }

  def createTaskFlow(taskFlow: UserExamStepAttemptTaskFlow): UserExamStepAttemptTaskFlow = {
    val nextId = if(stepAttemptTaskFlows.nonEmpty) stepAttemptTaskFlows.last.id + 1 else 1
    val withId = taskFlow.copy(id = nextId)
    stepAttemptTaskFlows += withId
    withId
  }

  def createTaskFlowStep(taskFlowStep: UserExamStepAttemptTaskFlowStep): UserExamStepAttemptTaskFlowStep = {
    val nextId = if(stepAttemptTaskFlowSteps.nonEmpty) stepAttemptTaskFlowSteps.last.id + 1 else 1
    val withId = taskFlowStep.copy(id = nextId)
    stepAttemptTaskFlowSteps += withId
    withId
  }
}
