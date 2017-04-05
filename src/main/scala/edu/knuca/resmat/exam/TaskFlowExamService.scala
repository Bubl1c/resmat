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

//  val chartXData = Array(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)
//
//  val task_flow_charts = ChartSet("Епюри", Seq(
//    ChartData("W Прогин (1/1000 м)",
//      chartXData,
//      Array(11.269, 10.733, 10.081, 9.268, 8.291, 7.160, 5.892, 4.511, 3.046, 1.530, 0.000),
//      true
//    ),
//    ChartData("{phi}{ Кут повороту (1/1000 рад)}",
//      chartXData,
//      Array(-5.280, -5.795, -7.301, -8.959, -10.567, -12.031, -13.286, -14.280, -14.965, -15.298, -15.233)
//    ),
//    ChartData("Mr Радіальний момент (кН)",
//      chartXData,
//      Array(0.000, 1.721, 1.960, 1.942, 1.822, 1.636, 1.398, 1.113, 0.784, 0.413, 0.000),
//      true
//    ),
//    ChartData("{M}{theta}{ Коловий момент (кН)}",
//      chartXData,
//      Array(4.685, 2.915, 2.551, 2.376, 2.240, 2.106, 1.964, 1.807, 1.632, 1.440, 1.229),
//      true
//    ),
//    ChartData("Qr Поперечна сила (кН/м)",
//      chartXData,
//      Array(0.000, -0.750, -1.333, -1.875, -2.400, -2.917, -3.429, -3.938, -4.444, -4.950, -5.455)
//    )
//  ))

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
    TaskFlowStepConf(3, 1, "Введіть пораховані значення невідомих X", 3,
      TaskFlowStepType.InputSet, InputSet(2, "InputSetName", Seq(
        InputSetInput(1, "X1", "", "м"),
        InputSetInput(2, "X2", "", "рад"),
        InputSetInput(3, "X3", "", "кНм/м"),
        InputSetInput(4, "X4", "", "кН/м")
      ), InputSetAnswerDto(2, Seq())).asJson.toString()
    ),
    TaskFlowStepConf(4, 1, "Епюри", 4, TaskFlowStepType.Charts, "will be retrieved from calc data", true),
    TaskFlowStepConf(5, 1, "Введіть пораховані значення", 5,
      TaskFlowStepType.InputSet, InputSet(3, "InputSetName", Seq(
        InputSetInput(1, "r", "Координати небезпечного перерізу", "м"),
        InputSetInput(2, "{sigma}{r}", "Радіального нормального напруження", "МПа"),
        InputSetInput(3, "{sigma}{theta}", "Колового нормального напруження", "МПа"),
        InputSetInput(4, "{sigma}{екв}", "Еквівалентного нормального напруження", "МПа"),
        InputSetInput(5, "{tau}{max}", "Максимальних дотичних напружень", "МПа")
      ), InputSetAnswerDto(3, Seq())).asJson.toString()
    ),
    TaskFlowStepConf(6, 1, "Чи забезпечується міцність перерізу?", 6, TaskFlowStepType.Test, TaskFlowTest(1000).asJson.toString()),
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
        val taskFlowTest = decode[TaskFlowTest](taskFlowStepConf.stepData).fold(er => None, test => Some(test)).getOrElse(
          throw new RuntimeException(s"Failed to parse test in $taskFlowStepConf")
        )
        val testConf = testSetExamService.getTestConfs(Seq(taskFlowTest.testId)).headOption.getOrElse(
          throw new RuntimeException(s"Test with id: ${taskFlowTest.testId}")
        )
        testConf.asJson.toString()
      case TaskFlowStepType.Charts =>
        val taskFlow = stepAttemptTaskFlows.find(_.id == taskFlowStep.stepAttemptTaskFlowId).getOrElse(
          throw new IllegalArgumentException(s"Task flow with id: ${taskFlowStep.stepAttemptTaskFlowId} not found!")
        )
        val problemVariantConf = problemVariantConfs.find(_.id == taskFlow.problemVariantConfId).getOrElse(
          throw new RuntimeException(s"Problem variant conf with id: ${taskFlow.problemVariantConfId} not found!")
        )
        //todo problem dependent - make independent
        val calcData = decode[RingPlateProblemResult](problemVariantConf.calculatedData).fold(_ => None, Some(_)).getOrElse(
          throw new RuntimeException(s"Failed to parse RingPlateProblemResult in ${problemVariantConf.calculatedData}")
        )
        ChartSet("Епюри", Seq(
          ChartData("W Прогин (1/1000 м)",
            calcData.r1,
            calcData.shiftAndForce.w_1,
            true
          ),
          ChartData("{phi}{ Кут повороту (1/1000 рад)}",
            calcData.r1,
            calcData.shiftAndForce.fi_1
          ),
          ChartData("Mr Радіальний момент (кН)",
            calcData.r1,
            calcData.shiftAndForce.mr_1,
            true
          ),
          ChartData("{M}{theta}{ Коловий момент (кН)}",
            calcData.r1,
            calcData.shiftAndForce.mt_1,
            true
          ),
          ChartData("Qr Поперечна сила (кН/м)",
            calcData.r1,
            calcData.shiftAndForce.qr_1
          )
        )).asJson.toString()
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
    val cd = decode[RingPlateProblemResult](problemVariantConf.calculatedData).fold(_ => None, Some(_)).getOrElse(
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
      def checkType(actual: TaskFlowStepType.TaskFlowStepType, required: TaskFlowStepType.TaskFlowStepType) =
        if(actual != required) throw new RuntimeException(s"Invalid task flow step type $actual, required $required")

      val stepAnswer: String = sc.id match {
        case 1 =>
          checkType(sc.stepType, TaskFlowStepType.Test)
          Seq(1).asJson.toString()
        case 2 =>
          checkType(sc.stepType, TaskFlowStepType.InputSet)
          Seq(
            InputSetInputAnswer(1, cd.extremeConditions.a.w),
            InputSetInputAnswer(2, cd.extremeConditions.a.fi),
            InputSetInputAnswer(3, cd.extremeConditions.a.mr),
            InputSetInputAnswer(4, cd.extremeConditions.a.qr),
            InputSetInputAnswer(5, cd.extremeConditions.b.w),
            InputSetInputAnswer(6, cd.extremeConditions.b.fi),
            InputSetInputAnswer(7, cd.extremeConditions.b.mr),
            InputSetInputAnswer(8, cd.extremeConditions.b.qr)
          ).asJson.toString()
        case 3 =>
          checkType(sc.stepType, TaskFlowStepType.InputSet)
          cd.gauss.b2.zipWithIndex.map{ case (e, i) => InputSetInputAnswer(i + 1, Some(e))}.asJson.toString()
        case 4 =>
          checkType(sc.stepType, TaskFlowStepType.Charts)
          ChartSet("Епюри", Seq(
            ChartData("W Прогин (1/1000 м)",
              cd.r1,
              cd.shiftAndForce.w_1,
              true
            ),
            ChartData("{phi}{ Кут повороту (1/1000 рад)}",
              cd.r1,
              cd.shiftAndForce.fi_1
            ),
            ChartData("Mr Радіальний момент (кН)",
              cd.r1,
              cd.shiftAndForce.mr_1,
              true
            ),
            ChartData("{M}{theta}{ Коловий момент (кН)}",
              cd.r1,
              cd.shiftAndForce.mt_1,
              true
            ),
            ChartData("Qr Поперечна сила (кН/м)",
              cd.r1,
              cd.shiftAndForce.qr_1
            )
          )).asJson.toString()
        case 5 =>
          checkType(sc.stepType, TaskFlowStepType.InputSet)
          Seq(
            InputSetInputAnswer(1, Some(cd.coordinateResult.r)),
            InputSetInputAnswer(2, Some(cd.coordinateResult.qr)),
            InputSetInputAnswer(3, Some(cd.coordinateResult.qt)),
            InputSetInputAnswer(4, Some(cd.coordinateResult.qeq)),
            InputSetInputAnswer(5, Some(cd.coordinateResult.tmax))
          ).asJson.toString()
        case 6 =>
          checkType(sc.stepType, TaskFlowStepType.Test)
          Seq(if(cd.isStrengthGuaranteed) 2 else 1).asJson.toString()
        case 7 =>
          checkType(sc.stepType, TaskFlowStepType.Finished)
          ""
      }
      createTaskFlowStep(UserExamStepAttemptTaskFlowStep(-1, taskFlow.id, sc.id, stepAnswer))
    }

    (taskFlow, taskFlowSteps)
  }

//  TaskFlowStepConf(3, 1, "Введіть пораховані значення невідомих X", 3,
//    TaskFlowStepType.InputSet, InputSet(2, "InputSetName", Seq(
//      InputSetInput(1, "X1", "На внутрішньому контурі", "м"),
//      InputSetInput(2, "X2", "На внутрішньому контурі", "рад"),
//      InputSetInput(3, "X3", "На внутрішньому контурі", "кНм/м"),
//      InputSetInput(4, "X4", "На внутрішньому контурі", "кН/м")
//    ), InputSetAnswerDto(2, Seq())).asJson.toString()
//  ),
//  TaskFlowStepConf(4, 1, "Епюри", 4, TaskFlowStepType.Charts, "will be retrieved from calc data", true),
//  TaskFlowStepConf(5, 1, "Введіть пораховані значення", 5,
//    TaskFlowStepType.InputSet, InputSet(3, "InputSetName", Seq(
//      InputSetInput(1, "r", "Координати небезпечного перерізу", "м"),
//      InputSetInput(2, "{sigma}{r}", "Радіального нормального напруження", "МПа"),
//      InputSetInput(3, "{sigma}{theta}", "Колового нормального напруження", "МПа"),
//      InputSetInput(4, "{sigma}{екв}", "Еквівалентного нормального напруження", "МПа"),
//      InputSetInput(5, "{tau}{max}", "Максимальних дотичних напружень", "МПа")
//    ), InputSetAnswerDto(3, Seq())).asJson.toString()
//  ),
//  TaskFlowStepConf(6, 1, "Чи забезпечується міцність перерізу?", 6, TaskFlowStepType.Test, TaskFlowTest(1000).asJson.toString()),
//  TaskFlowStepConf(7, 1, "Кінець", 7, TaskFlowStepType.Finished, "")

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
