package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.core._
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

  val problemConfs: List[ProblemConf] = List(
    ProblemConf(1, "Кільцева пластина", ProblemType.RingPlate, Seq(
      VarConf(2, "Fa", "кН/м", "a.f"),
      VarConf(3, "Ma", "кНм/м", "a.m"),
      VarConf(4, "wa", "м", "a.w"),
      VarConf(5, "{phi}{a}", "рад", "a.fi"),
    
      VarConf(6, "E", "МПа", "moduleE"),
      VarConf(7, "{mu}", "", "poissonRatio"),
      VarConf(8, "q", "кН/м^2", "q"),
    
      VarConf(9, "Fb", "кН/м", "b.f"),
      VarConf(10, "Mb", "кНм/м", "b.m"),
      VarConf(11, "wb", "м", "b.w"),
      VarConf(12, "{phi}{b}", "рад", "b.fi"),
    
      VarConf(13, "a", "м", "a.length"),
      VarConf(14, "b", "м", "b.length"),
      VarConf(15, "t", "м", "height"),

      VarConf(16, "an", "", "a.n", false),
      VarConf(17, "bn", "", "b.n", false),
      VarConf(18, "sigmaAdm", "", "sigmaAdm", false)
    ).asJson.toString())
  )

  val varVals: List[ProblemInputVariableValue] = List(
    VarVal(2, 0),
    VarVal(3, 0),
    VarVal(4, -0.01),
    VarVal(5, 0),

    VarVal(6, 200000000.0),
    VarVal(7, 0.3),
    VarVal(8, 0d),

    VarVal(9, 0),
    VarVal(10, 0),
    VarVal(11, 0),
    VarVal(12, 0),

    VarVal(13, 0.1),
    VarVal(14, 1.1),
    VarVal(15, 0.02),

    VarVal(16, 1),
    VarVal(17, 2),
    VarVal(18, 160)
  )

  val decodedConfs = decode[Seq[VarConf]](problemConfs.find(_.id == 1).get.inputVariableConfs).fold(_ => None, Some(_)).getOrElse(
    throw new RuntimeException("Shit is happening")
  )
  val variant1Input = RingPlateProblemInput(decodedConfs.map(pc => {
    val varVal = varVals.find(_.variableConfId == pc.id).get
    (pc, varVal)
  }))
  val problemVariantConfs: List[ProblemVariantConf] = List(
    ProblemVariantConf(1, 1, "img/tasks/9.png",
      varVals.asJson.toString(),
      new RingPlateSolver(variant1Input).solve().asJson.toString()
    )
  )

  val taskFlowConfs: List[TaskFlowConf] = List(
    TaskFlowConf(1, 1)
  )

  import edu.knuca.resmat.core.RingPlateProblemAnswer.{Mapping => M}

  val taskFlowStepConfs: List[TaskFlowStepConf] = List(
    TaskFlowStepConf(1, 1, "Визначення типу пластини", 1, TaskFlowStepType.Test,
      TaskFlowTestConf(TestConf(-1, -1, "Визначте тип ластини", Seq(
        TestOptionConf(1, "Тонкі", true),
        TestOptionConf(2, "Товсті"),
        TestOptionConf(3, "Мембрани")
      ))).asJson.toString()
    ),
    TaskFlowStepConf(2, 1, "Введіть значення граничних умов, якщо умова невідома - залиште поле пустим", 2,
      TaskFlowStepType.InputSet, InputSet(1, "InputSetName", Seq(
        InputSetInput(1, "w(a)", "На внутрішньому контурі", "м", M.w_a),
        InputSetInput(2, "{phi}{(a)}", "На внутрішньому контурі", "рад", M.fi_a),
        InputSetInput(3, "Mr(a)", "На внутрішньому контурі", "кНм/м", M.mr_a),
        InputSetInput(4, "Qr(a)", "На внутрішньому контурі", "кН/м", M.qr_a),
        InputSetInput(5, "w(b)", "На зовнішньому контурі", "м", M.w_b),
        InputSetInput(6, "{phi}{(b)}", "На зовнішньому контурі", "рад", M.fi_b),
        InputSetInput(7, "Mr(b)", "На зовнішньому контурі", "кНм/м", M.mr_b),
        InputSetInput(8, "Qr(b)", "На зовнішньому контурі", "кН/м", M.qr_b)
      )).asJson.toString()
    ),
    TaskFlowStepConf(3, 1, "Введіть пораховані значення невідомих X", 3,
      TaskFlowStepType.InputSet, InputSet(2, "InputSetName", Seq(
        InputSetInput(1, "X1", "", "м", M.x1),
        InputSetInput(2, "X2", "", "рад", M.x2),
        InputSetInput(3, "X3", "", "кНм/м", M.x3),
        InputSetInput(4, "X4", "", "кН/м", M.x4)
      )).asJson.toString()
    ),
    TaskFlowStepConf(4, 1, "Епюри", 4, TaskFlowStepType.Charts, M.charts, true),
    TaskFlowStepConf(5, 1, "Введіть пораховані значення", 5,
      TaskFlowStepType.InputSet, InputSet(3, "InputSetName", Seq(
        InputSetInput(1, "r", "Координати небезпечного перерізу", "м", M.r),
        InputSetInput(2, "{sigma}{r}", "Радіального нормального напруження", "МПа", M.sigma_r),
        InputSetInput(3, "{sigma}{theta}", "Колового нормального напруження", "МПа", M.sigma_theta),
        InputSetInput(4, "{sigma}{екв}", "Еквівалентного нормального напруження", "МПа", M.sigma_eq),
        InputSetInput(5, "{tau}{max}", "Максимальних дотичних напружень", "МПа", M.tau_max)
      )).asJson.toString()
    ),
    TaskFlowStepConf(6, 1, "Чи забезпечується міцність перерізу?", 6, TaskFlowStepType.Test,
      TaskFlowTestConf(TestConf(-1, -1, "Чи забезпечуться міцність перерізу?", Seq(
        TestOptionConf(0, "Не забезпечується"),
        TestOptionConf(1, "Забезпечується")
      )), Some(M.isStrengthGuranteed)).asJson.toString()
    ),
    TaskFlowStepConf(7, 1, "Кінець", 7, TaskFlowStepType.Finished, "")
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
        val taskFlowTest = decode[TaskFlowTestConf](taskFlowStepConf.stepData).fold(er => None, test => Some(test)).getOrElse(
          throw new RuntimeException(s"Failed to parse test in $taskFlowStepConf")
        )
        taskFlowTest.test.asJson.toString()
      case TaskFlowStepType.Charts =>
        taskFlowStep.answer
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
        val correctAnswer = decode[Seq[Long]](taskFlowStep.answer).fold(er => None, test => Some(test)).getOrElse(
          throw new RuntimeException(s"Failed to parse test answer in $answer")
        )
        val verifiedAnswer = testSetExamService.verifyTestAnswer(testAnswer, correctAnswer)
        updateTaskFlowStep(taskFlowStep, verifiedAnswer.isCorrectAnswer, verifiedAnswer.mistakesAmount)
        if(verifiedAnswer.isCorrectAnswer) updateTaskFlowCurrentStep(taskFlow.id)
        Some(
          VerifiedTaskFlowStepAnswer(
            verifiedAnswer.isCorrectAnswer,
            verifiedAnswer.mistakesAmount,
            verifiedAnswer.answer.asJson.toString()
          )
        )
      case TaskFlowStepType.InputSet =>
        val inputSetAnswer = decode[InputSetAnswerDto](answer).fold(er => None, is => Some(is)).getOrElse(
          throw new RuntimeException(s"Failed to parse data in $taskFlowStepConf")
        )
        val correctAnswer = decode[Seq[InputSetInputAnswer]](taskFlowStep.answer).fold(er => None, test => Some(test)).getOrElse(
          throw new RuntimeException(s"Failed to parse test answer in $answer")
        )
        val verifiedAnswer = verifyInputSet(inputSetAnswer, correctAnswer)
        updateTaskFlowStep(taskFlowStep, verifiedAnswer.isCorrectAnswer, verifiedAnswer.mistakesAmount)
        if(verifiedAnswer.isCorrectAnswer) updateTaskFlowCurrentStep(taskFlow.id)
        Some(
          VerifiedTaskFlowStepAnswer(
            verifiedAnswer.isCorrectAnswer,
            verifiedAnswer.mistakesAmount,
            verifiedAnswer.answer.asJson.toString()
          )
        )
      case _ => throw new IllegalArgumentException(s"Unsupported task flow step to verify: ${taskFlowStepConf.stepType}")
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

  def verifyInputSet(submittedAnswer: InputSetAnswerDto, correctAnswer: Seq[InputSetInputAnswer]): VerifiedInputSetAnswer = {
    var isCorrectAnswer = true
    var mistakesAmount = 0
    val verifiedAnswers: Map[Int, Boolean] = correctAnswer.map{ correctAnswer =>
      submittedAnswer.inputAnswers.find(_.id == correctAnswer.id) match {
        case Some(submittedInputAnswer) =>
          val areEqual = submittedInputAnswer.value match {
            case Some(sia) => correctAnswer.value match {
              case Some(ca) => areAlmostEqual(ca, sia)
              case None => false
            }
            case None => correctAnswer.value match {
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
    VerifiedInputSetAnswer(submittedAnswer.inputSetId, isCorrectAnswer, mistakesAmount, verifiedAnswers)
  }

  def areAlmostEqual(ethalon: Double, d2: Double, precision: Double = 0.05): Boolean = {
    val diff = (ethalon - d2).abs
    if(ethalon == 0.0) {
      ethalon == d2
    } else {
      (diff / ethalon).abs <= precision
    }
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
    val problemVariantConf = problemVariantConfs.find(_.id == taskFlowConfProblemVariantConf.problemVariantConfId).getOrElse(
      throw new RuntimeException(s"Problem variant conf with id: ${taskFlowConfProblemVariantConf.problemVariantConfId} not found!")
    )
    val cd = decode[RingPlateProblemAnswer](problemVariantConf.calculatedData).fold(_ => None, Some(_)).getOrElse(
      throw new RuntimeException(s"Failed to parse RingPlateProblemResult from calculated variant data ${problemVariantConf.calculatedData}")
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
      val stepAnswer: String = sc.stepType match {
        case TaskFlowStepType.Test =>
          val stepTestConf = decode[TaskFlowTestConf](sc.stepData).fold(_ => None, Some(_)).getOrElse(
            throw new RuntimeException(s"Failed to parse TaskFlowTestConf from step data ${sc.stepData}")
          )
          stepTestConf.correctOptionIdsMapping
            .fold(stepTestConf.test.getCorrectOptionIds)(mapping =>
              cd.getString(mapping).split(",").map(_.toLong)
            ).asJson.toString()
        case TaskFlowStepType.InputSet =>
          val stepInputSet = decode[InputSet](sc.stepData).fold(_ => None, Some(_)).getOrElse(
            throw new RuntimeException(s"Failed to parse InputSet from step data ${sc.stepData}")
          )
          stepInputSet.inputs.map(input =>
            InputSetInputAnswer(input.id, cd.getDoubleOpt(input.answerMapping))
          ).asJson.toString()
        case TaskFlowStepType.Charts =>
          cd.get(sc.stepData) match {
            case chartSet: ChartSet => chartSet.asJson.toString()
            case _ => throw new RuntimeException(s"Failed to get ChartSet with key ${sc.stepData}")
          }
        case TaskFlowStepType.Finished =>
          ""
        case st => throw new RuntimeException(s"Unhandled step type: $st")
      }
      createTaskFlowStep(UserExamStepAttemptTaskFlowStep(-1, taskFlow.id, sc.id, stepAnswer))
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
