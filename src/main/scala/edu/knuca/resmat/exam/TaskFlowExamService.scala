package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

import edu.knuca.resmat.exam.{ProblemInputVariableConf => VarConf}
import edu.knuca.resmat.exam.{ProblemInputVariableValue => VarVal}

import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

case class TaskFlowDto(problemConf: ProblemConf,
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

  val chartXData = Array(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)

  val chartYData = Array(
    Array(11.269, 10.733, 10.081, 9.268, 8.291, 7.160, 5.892, 4.511, 3.046, 1.530, 0.000),
    Array(-5.280, -5.795, -7.301, -8.959, -10.567, -12.031, -13.286, -14.280, -14.965, -15.298, -15.233),
    Array(0.000, 1.721, 1.960, 1.942, 1.822, 1.636, 1.398, 1.113, 0.784, 0.413, 0.000),
    Array(4.685, 2.915, 2.551, 2.376, 2.240, 2.106, 1.964, 1.807, 1.632, 1.440, 1.229),
    Array(0.000, -0.750, -1.333, -1.875, -2.400, -2.917, -3.429, -3.938, -4.444, -4.950, -5.455)
  )

  val task_flow_charts = ChartSet("Епюри", Seq(
    ChartData("W Прогин (1/1000 м)",
      chartXData,
      Array(11.269, 10.733, 10.081, 9.268, 8.291, 7.160, 5.892, 4.511, 3.046, 1.530, 0.000),
      true
    ),
    ChartData("{phi}{ Кут повороту (1/1000 рад)}",
      chartXData,
      Array(-5.280, -5.795, -7.301, -8.959, -10.567, -12.031, -13.286, -14.280, -14.965, -15.298, -15.233)
    ),
    ChartData("Mr Радіальний момент (кН)",
      chartXData,
      Array(0.000, 1.721, 1.960, 1.942, 1.822, 1.636, 1.398, 1.113, 0.784, 0.413, 0.000),
      true
    ),
    ChartData("{M}{theta}{ Коловий момент (кН)}",
      chartXData,
      Array(4.685, 2.915, 2.551, 2.376, 2.240, 2.106, 1.964, 1.807, 1.632, 1.440, 1.229),
      true
    ),
    ChartData("Qr Поперечна сила (кН/м)",
      chartXData,
      Array(0.000, -0.750, -1.333, -1.875, -2.400, -2.917, -3.429, -3.938, -4.444, -4.950, -5.455)
    )
  ))

  val problemConfs: List[ProblemConf] = List(
    ProblemConf(1, "Кільцева пластина", Seq(
      VarConf(2, "Fa", "кН/м"),
      VarConf(3, "Ma", "кНм/м"),
      VarConf(4, "wa", "м"),
      VarConf(5, "{phi}{a}", "рад"),
    
      VarConf(6, "E", "МПа"),
      VarConf(7, "{mu}"),
      VarConf(8, "q", "кН/м^2"),
    
      VarConf(9, "Fb", "кН/м"),
      VarConf(10, "Mb", "кНм/м"),
      VarConf(11, "wb", "м"),
      VarConf(12, "{phi}{b}", "рад"),
    
      VarConf(13, "a", "м"),
      VarConf(14, "b", "м"),
      VarConf(15, "t", "мм")
    ))
  )
  val problemVariantConfs: List[ProblemVariantConf] = List(
    ProblemVariantConf(1, 1, "img/tasks/9.png", Seq(
      VarVal(2, 0),
      VarVal(3, 0),
      VarVal(4, 0),
      VarVal(5, 0),

      VarVal(6, 100000),
      VarVal(7, 0.2),
      VarVal(8, 10),

      VarVal(9, 0),
      VarVal(10, 0),
      VarVal(11, 0),
      VarVal(12, 0),

      VarVal(13, 0.1),
      VarVal(14, 1.1),
      VarVal(15, 22)
    ))
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
        InputSetInputAnswer(8, Some(5))
      ))).asJson.toString()
    ),
    TaskFlowStepConf(3, 1, "Епюри", 3, TaskFlowStepType.Charts, task_flow_charts.asJson.toString(), true),
    TaskFlowStepConf(4, 1, "Чи забезпечується міцність перерізу?", 4, TaskFlowStepType.Test, TaskFlowTest(1000).asJson.toString()),
    TaskFlowStepConf(5, 1, "Епюри", 5, TaskFlowStepType.Finished, "")
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
    val problemVariantConf = problemVariantConfs.find(_.id == taskFlow.problemVariantConfId).getOrElse(
      throw new RuntimeException(s"Problem variant conf with id: ${taskFlow.problemVariantConfId} not found!")
    )
    val problemConf = problemConfs.find(_.id == problemVariantConf.problemConfId).getOrElse(
      throw new RuntimeException(s"Problem conf with id: ${problemVariantConf.problemConfId} not found!")
    )
    Some(TaskFlowDto(problemConf, problemVariantConf, taskFlow))
  }

  def getCurrentTaskFlowStep(taskFlowId: Long): Option[TaskFlowStepDto] = {
    val taskFlow = stepAttemptTaskFlows.find(_.id == taskFlowId).getOrElse(
      throw new IllegalArgumentException(s"Task flow with id: $taskFlowId not found!")
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
    if(taskFlowStepConf.helpData) {
      updateTaskFlowStep(taskFlowStep.copy(done = true))
      updateTaskFlowCurrentStep(taskFlowId)
    }
    val taskFlowStepData: String = taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val taskFlowTest = decode[TaskFlowTest](taskFlowStepConf.stepData).fold(er => None, test => Some(test)).getOrElse(
          throw new RuntimeException(s"Failed to parse test in $taskFlowStepConf")
        )
        val testConf = testSetExamService.getTestConfs(Seq(taskFlowTest.testId)).headOption.getOrElse(
          throw new RuntimeException(s"Test with id: ${taskFlowTest.testId}")
        )
        testConf.asJson.toString()
      case TaskFlowStepType.Finished =>
        updateTaskFlowStep(taskFlowStep.copy(done = true))
        "Task flow has been finished successfully".asJson.toString()
      case _ => taskFlowStepConf.stepData
    }
    Some(TaskFlowStepDto(taskFlowStepConf, taskFlowStep, taskFlowStepData))
  }

  def verifyTaskFlowStepAnswer(taskFlowStepId: Long, answer: String): Option[VerifiedTaskFlowStepAnswer] = {

    val taskFlowStep = stepAttemptTaskFlowSteps.find(s => s.id == taskFlowStepId).getOrElse(
      throw new RuntimeException(s"Task flow step with id: $taskFlowStepId not found!")
    )
    val taskFlow = stepAttemptTaskFlows.find(_.id == taskFlowStep.stepAttemptTaskFlowId).getOrElse(
      throw new RuntimeException(s"TaskFlow with id: ${taskFlowStep.stepAttemptTaskFlowId} not found!")
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
          if(va.isCorrectAnswer) updateTaskFlowCurrentStep(taskFlow.id)
          VerifiedTaskFlowStepAnswer(va.isCorrectAnswer, va.mistakesAmount, va.answer.asJson.toString())
        }
      case TaskFlowStepType.InputSet =>
        val inputSetAnswer = decode[InputSetAnswerDto](answer).fold(er => None, is => Some(is)).getOrElse(
          throw new RuntimeException(s"Failed to parse data in $taskFlowStepConf")
        )
        val inputSet = decode[InputSet](taskFlowStepConf.stepData).fold(er => None, is => Some(is)).getOrElse(
          throw new RuntimeException(s"Failed to parse input set in $taskFlowStepConf")
        )
        val verifiedAnswer = verifyInputSet(taskFlow.problemVariantConfId, inputSetAnswer, inputSet)
        verifiedAnswer.map{ va =>
          updateTaskFlowStep(taskFlowStep, va.isCorrectAnswer, va.mistakesAmount)
          if(va.isCorrectAnswer) updateTaskFlowCurrentStep(taskFlow.id)
          VerifiedTaskFlowStepAnswer(va.isCorrectAnswer, va.mistakesAmount, va.answer.asJson.toString())
        }
      case TaskFlowStepType.Charts =>
        Some(VerifiedTaskFlowStepAnswer(true, 0, "not implemented"))
    }
  }

  def updateTaskFlowCurrentStep(taskFlowId: Long): UserExamStepAttemptTaskFlow = {
    val index = stepAttemptTaskFlows.indexWhere(_.id == taskFlowId)
    val taskFlow = stepAttemptTaskFlows(index)
    val steps = stepAttemptTaskFlowSteps.filter(_.stepAttemptTaskFlowId == taskFlowId)
    val stepConfs = taskFlowStepConfs.filter(_.taskFlowConfId == taskFlow.taskFlowConfId)
    val currentStepConf = steps.find(_.id == taskFlow.currentStepId).flatMap( step =>
      stepConfs.find(_.id == step.taskFlowStepConfId)
    )
    val nextStepConf = currentStepConf.flatMap(csc => stepConfs.find(_.sequence == csc.sequence + 1))
    val nextStep = nextStepConf.flatMap(nsc => steps.find(_.taskFlowStepConfId == nsc.id))
    nextStep.foreach( ns =>
      stepAttemptTaskFlows.update(index, taskFlow.copy(currentStepId = ns.id))
    )
    taskFlow
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
          val areEqual = inputSetInputAnswer.value match {
            case Some(isa) => correctAnswer.value match {
              case Some(ca) => areAlmostEqual(ca, isa)
              case None => false
            }
            case None => inputSetInputAnswer.value match {
              case Some(ca) => false
              case None => true
            }
          }
          if(areEqual) {
            (correctAnswer.id, true)
          } else {
            isCorrectAnswer = false
            mistakesAmount = mistakesAmount + 1
            (correctAnswer.id, false)
          }
        case None =>
          isCorrectAnswer = false
          mistakesAmount = mistakesAmount + 1
          (correctAnswer.id, false)
      }
    }.toMap
    Some(VerifiedInputSetAnswer(inputSetAnswer.inputSetId, isCorrectAnswer, mistakesAmount, verifiedAnswers))
  }

  def areAlmostEqual(d1: Double, d2: Double, precision: Double = 0.0001): Boolean = (d1 - d2).abs <= precision

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
