package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.core._
import edu.knuca.resmat.db.DatabaseService
import io.circe.{Decoder, Error}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
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
                         (val problemService: ProblemService)
                         (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.http.JsonProtocol._

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

  case class ResourceNotFoundException(message: String) extends IllegalArgumentException {
    override def getMessage: String = super.getMessage + " " + message
  }

  def getTaskFlowById(id: Long): UserExamStepAttemptTaskFlow =
    stepAttemptTaskFlows.find(_.id == id).getOrElse(
      throw ResourceNotFoundException(s"Task flow for with id: $id not found!")
    )

  def getTaskFlowByStepAttemptId(stepAttemptId: Long): UserExamStepAttemptTaskFlow =
    stepAttemptTaskFlows.find(_.stepAttemptId == stepAttemptId).getOrElse(
      throw ResourceNotFoundException(s"Task flow for step attempt id: $stepAttemptId not found!")
    )

  def getTaskFlowStepConfById(id: Long): TaskFlowStepConf = taskFlowStepConfs.find(_.id == id).getOrElse(
    throw new RuntimeException(s"Task flow step conf with id: $id not found!")
  )

  def getTaskFlowStepById(id: Long): UserExamStepAttemptTaskFlowStep =
    stepAttemptTaskFlowSteps.find(_.id == id).getOrElse(
      throw new RuntimeException(s"Task flow step with id: $id not found!")
    )

  def getTaskFlowConfProblemVariantConfById(id: Long): TaskFlowConfProblemVariantConf = {
    taskFlowConfProblemVariantConfs.find(_.id == id).getOrElse(
      throw new RuntimeException(s"Task flow conf problem variant conf with id: $id not found!")
    )
  }

  def getTaskFlowStepConfsByTaskFlowConfId(taskFlowConfId: Long): Seq[TaskFlowStepConf] = {
    taskFlowStepConfs.filter(_.taskFlowConfId == taskFlowConfId)
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

  def updateTaskFlowStep(taskFlowStep: UserExamStepAttemptTaskFlowStep): UserExamStepAttemptTaskFlowStep = {
    val index = stepAttemptTaskFlowSteps.indexWhere(_.id == taskFlowStep.id)
    stepAttemptTaskFlowSteps.update(index, taskFlowStep)
    taskFlowStep
  }

  def getNotCompletedTaskFlowSteps(stepAttemptId: Long): Seq[UserExamStepAttemptTaskFlowStep] = {
    val taskFlow = getTaskFlowByStepAttemptId(stepAttemptId)
    stepAttemptTaskFlowSteps.filter(s => s.stepAttemptTaskFlowId == taskFlow.id && !s.done)
  }

  def getTaskFlowDto(stepAttemptId: Long): Option[TaskFlowDto] = {
    val taskFlow = getTaskFlowByStepAttemptId(stepAttemptId)
    val problemVariantConf = problemService.getProblemVariantConfById(taskFlow.problemVariantConfId)
    val problemConf = problemService.getProblemConfById(problemVariantConf.problemConfId)
    Some(TaskFlowDto(problemConf, problemVariantConf, taskFlow))
  }

  def getCurrentTaskFlowStep(taskFlowId: Long): Option[TaskFlowStepDto] = {
    val taskFlow = getTaskFlowById(taskFlowId)
    getTaskFlowStep(taskFlow.id, taskFlow.currentStepId)
  }

  def getTaskFlowStep(taskFlowId: Long, taskFlowStepId: Long): Option[TaskFlowStepDto] = {
    val taskFlowStep = getTaskFlowStepById(taskFlowStepId)
    val taskFlowStepConf = getTaskFlowStepConfById(taskFlowStep.taskFlowStepConfId)
    if(taskFlowStepConf.helpData) {
      updateTaskFlowStep(taskFlowStep.copy(done = true))
      updateTaskFlowCurrentStep(taskFlowId)
    }
    val taskFlowStepData: String = taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val taskFlowTest = decodeOrElse[TaskFlowTestConf](taskFlowStepConf.stepData)(
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
    val taskFlowStep = getTaskFlowStepById(taskFlowStepId)
    val taskFlow = getTaskFlowById(taskFlowStep.stepAttemptTaskFlowId)
    val taskFlowStepConf = getTaskFlowStepConfById(taskFlowStep.taskFlowStepConfId)
    taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val testAnswer = decodeOrElse[TestAnswerDto](answer)(
          throw new RuntimeException(s"Failed to parse test answer in $answer")
        )
        val correctAnswer = decodeOrElse[Seq[Long]](taskFlowStep.answer)(
          throw new RuntimeException(s"Failed to parse test answer in $answer")
        )
        val verifiedAnswer = TestUtils.verify(testAnswer, correctAnswer)
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
        val inputSetAnswer = decodeOrElse[InputSetAnswerDto](answer)(
          throw new RuntimeException(s"Failed to parse data in $taskFlowStepConf")
        )
        val correctAnswer = decodeOrElse[Seq[InputSetInputAnswer]](taskFlowStep.answer)(
          throw new RuntimeException(s"Failed to parse test answer in $answer")
        )
        val verifiedAnswer = InputSetUtils.verify(inputSetAnswer, correctAnswer)
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

  def updateTaskFlowStep(taskFlowStep: UserExamStepAttemptTaskFlowStep,
                         isCorrectAnswer: Boolean,
                         mistakesAmount: Int): UserExamStepAttemptTaskFlowStep = {
    if(isCorrectAnswer) {
      updateTaskFlowStep(taskFlowStep.copy(done = true))
    } else {
      updateTaskFlowStep(taskFlowStep.copy(mistakes = taskFlowStep.mistakes + mistakesAmount))
    }
  }

  def createTaskFlowWithSteps(stepAttemptId: Long,
                              userExamId: Long,
                              examStepConfId: Long,
                              taskFlowConfProblemVariantConfId: Long): (UserExamStepAttemptTaskFlow, Seq[UserExamStepAttemptTaskFlowStep]) = {
    val taskFlowConfProblemVariantConf = getTaskFlowConfProblemVariantConfById(taskFlowConfProblemVariantConfId)
    val problemVariantConf = problemService.getProblemVariantConfById(taskFlowConfProblemVariantConf.problemVariantConfId)
    val cd = decodeOrElse[RingPlateProblemAnswer](problemVariantConf.calculatedData)(
      throw new RuntimeException(s"Failed to parse RingPlateProblemResult from calculated variant data ${problemVariantConf.calculatedData}")
    )
    val stepConfs = getTaskFlowStepConfsByTaskFlowConfId(taskFlowConfProblemVariantConf.taskFlowConfId)

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
          val stepTestConf = decodeOrElse[TaskFlowTestConf](sc.stepData)(
            throw new RuntimeException(s"Failed to parse TaskFlowTestConf from step data ${sc.stepData}")
          )
          stepTestConf.correctOptionIdsMapping
            .fold(stepTestConf.test.getCorrectOptionIds)(mapping =>
              cd.getString(mapping).split(",").map(_.toLong)
            ).asJson.toString()
        case TaskFlowStepType.InputSet =>
          val stepInputSet = decodeOrElse[InputSet](sc.stepData)(
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

  private def decodeOrElse[A: Decoder](input: String)(default: () => A): A =
    decode[A](input).fold(_ => None, Some(_)).getOrElse(default())
}
