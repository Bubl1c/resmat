package edu.knuca.resmat.exam.taskflow

import java.sql.Connection

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.core._
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.testset.{TestSingleInputSubmittedAnswer, TestSubmittedAnswerDto, TestUtils}
import edu.knuca.resmat.utils.{CollectionUtils, SqlUtils}
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import edu.knuca.resmat.http.JsonProtocol._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

case class TaskFlowDto(problemConf: ProblemConf,
                       problemVariantConf: PublicProblemVariantConf,
                       taskFlow: UserExamStepAttemptTaskFlow) extends StepDataDto
case class TaskFlowStepDto(taskFlowStepConf: TaskFlowStepConf,
                           stepAttemptTaskFlowStep: UserExamStepAttemptTaskFlowStep,
                           taskFlowStepData: String,
                           helpSteps: Seq[HelpStepDataDto])
case class HelpStepDataDto(id: Long, stepType: TaskFlowStepType.TaskFlowStepType, name: String, data: String)
case class TaskFlowResultInfoStepDataDto(id: Long, stepType: TaskFlowStepType.TaskFlowStepType, name: String, data: String)

case class InputSetAnswerDto(inputSetId: Long, inputAnswers: Seq[InputSetInputAnswer])
case class InputSetInputAnswer(id: Int, value: Option[Double] = None)

case class VerifiedTaskFlowStepAnswer(isCorrectAnswer: Boolean, mistakesAmount: Int, answer: String)

case class VerifiedInputSetAnswer(inputSetId: Long, isCorrectAnswer: Boolean, mistakesAmount: Int, answer: Map[Int, Boolean])

case class ResourceNotFoundException(message: String) extends IllegalArgumentException {
  override def getMessage: String = super.getMessage + " " + message
}

class TaskFlowConfAndExamService(val db: DatabaseService)
                                (val problemService: ProblemConfService)
                                (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.exam.taskflow.{TaskFlowQueries => Q}

  //====================TaskFlowConf====================

  def createTaskFlowConfWithSteps(data: TaskFlowConfDto): TaskFlowConfDto = {
    val createdId = db.runTransaction { implicit c =>
      createTaskFlowConfWithStepsTransact(data)
    }
    getTaskFlowConfDto(createdId)
  }

  def createTaskFlowConfWithStepsTransact(data: TaskFlowConfDto)(implicit c: Connection): Long = {
    val createdTfcId = createTaskFlowConfTransact(data.taskFlowConf)
    val createdTfsConfIds = data.taskFlowSteps.map( tfsc =>
      createTaskFlowStepConfTransact(
        tfsc.copy(taskFlowConfId = createdTfcId)
      )
    )
    createdTfcId
  }

  def createTaskFlowConf(taskFlowConf: TaskFlowConf): TaskFlowConf = db.run { implicit c =>
    val insertedId = createTaskFlowConfTransact(taskFlowConf)
    getTaskFlowConf(insertedId)
  }

  def createTaskFlowConfTransact(taskFlowConf: TaskFlowConf)(implicit c: Connection): Long = {
    val insertedIdOpt: Option[Long] = Q.createTaskFlowConf(taskFlowConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $taskFlowConf")
    )
    insertedId
  }

  def updateTaskFlowConfWithStepsTransact(taskFlowConfId: Long, data: TaskFlowConfDto)(implicit c: Connection): Unit = {
    updateTaskFlowConfTransact(taskFlowConfId, data.taskFlowConf)
    val currentStepConfs = findTaskFlowStepConfsTransact(taskFlowConfId)
    val diff = CollectionUtils.diff[TaskFlowStepConf, TaskFlowStepConf](
      currentStepConfs,
      data.taskFlowSteps,
      (a, b) => a.id == b.id
    )
    val createdIds = diff.added.map(tfsc => createTaskFlowStepConfTransact(tfsc))
    diff.same.foreach(tfsc => updateTaskFlowStepConfTransact(tfsc.id, tfsc))
    deleteTaskFlowStepConfsTransact(diff.removed.map(_.id))
  }

  def updateTaskFlowConfTransact(id: Long, taskFlowConf: TaskFlowConf)(implicit c: Connection): Unit = {
    val updatedRows: Int = Q.updateTaskFlowConf(id, taskFlowConf).executeUpdate()
    if(updatedRows == 0) {
      throw new RuntimeException(s"Failed to update task flow conf by id $id")
    }
    if(updatedRows > 1) {
      throw new RuntimeException(s"Updated $updatedRows rows while updating task flow conf by id $id")
    }
  }

  def getTaskFlowConfDto(id: Long): TaskFlowConfDto = db.run { implicit c =>
    val tfc = getTaskFlowConfTransact(id)
    val steps = findTaskFlowStepConfs(id)
    TaskFlowConfDto(tfc, steps)
  }

  def getTaskFlowConf(id: Long): TaskFlowConf = db.run { implicit c =>
    getTaskFlowConfTransact(id)
  }

  def getTaskFlowConfTransact(id: Long)(implicit c: Connection): TaskFlowConf = {
    Q.getTaskFlowConf(id).as(Q.tfcParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Task flow conf with id: $id not found!")
    )
  }

  def deleteTaskFlowConfTransact(id: Long)(implicit c: Connection): Unit = {
    val updatedRows: Int = Q.deleteTaskFlowConf(id).executeUpdate()
    if(updatedRows == 0) {
      throw new RuntimeException(s"Failed to delete task flow conf by id $id")
    }
    if(updatedRows > 1) {
      throw new RuntimeException(s"Deleted $updatedRows rows while deleting task flow conf by id $id")
    }
  }

  //====================TaskFlowStepConf====================

  def createTaskFlowStepConf(taskFlowStepConf: TaskFlowStepConf): TaskFlowStepConf = db.run { implicit c =>
    val insertedId = createTaskFlowStepConfTransact(taskFlowStepConf)
    getTaskFlowStepConf(insertedId)
  }

  def createTaskFlowStepConfTransact(taskFlowStepConf: TaskFlowStepConf)(implicit c: Connection): Long = {
    val insertedIdOpt: Option[Long] = Q.createTaskFlowStepConf(taskFlowStepConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $taskFlowStepConf")
    )
    insertedId
  }

  def updateTaskFlowStepConfTransact(id: Long, taskFlowStepConf: TaskFlowStepConf)(implicit c: Connection): Unit = {
    val updatedRows: Int = Q.updateTaskFlowStepConf(id, taskFlowStepConf).executeUpdate()
    if(updatedRows == 0) {
      throw new RuntimeException(s"Failed to update task flow step conf by id $id")
    }
    if(updatedRows > 1) {
      throw new RuntimeException(s"Updated $updatedRows rows while updating task flow step conf by id $id")
    }
  }

  def deleteTaskFlowStepConfsTransact(ids: Seq[Long])(implicit c: Connection): Unit = {
    val deletedRows: Int = Q.deleteTaskFlowStepConfs(ids).executeUpdate()
    if(deletedRows != ids.size) {
      throw new RuntimeException(s"Failed to delete task flow step confs by ids [${ids.mkString(", ")}]")
    }
  }

  def getTaskFlowStepConf(id: Long): TaskFlowStepConf = db.run { implicit c =>
    getTaskFlowStepConfTransact(id)
  }

  def getTaskFlowStepConfTransact(id: Long)(implicit c: Connection): TaskFlowStepConf = {
    Q.getTaskFlowStepConf(id).as(Q.tfscParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Task flow step conf with id: $id not found!")
    )
  }

  def findTaskFlowStepConfs(taskFlowConfId: Long): Seq[TaskFlowStepConf] = db.run { implicit c =>
    Q.findTaskFlowStepConfs(taskFlowConfId).as(Q.tfscParser.*)
  }

  def findTaskFlowStepConfsTransact(taskFlowConfId: Long)(implicit c: Connection): Seq[TaskFlowStepConf] = {
    Q.findTaskFlowStepConfs(taskFlowConfId).as(Q.tfscParser.*)
  }

  //====================UserExamStepAttemptTaskFlow====================

  def createTaskFlow(taskFlow: UserExamStepAttemptTaskFlow): UserExamStepAttemptTaskFlow = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTaskFlow(taskFlow).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $taskFlow")
    )
    getTaskFlow(insertedId)
  }

  def getTaskFlow(id: Long): UserExamStepAttemptTaskFlow = db.run { implicit c =>
    Q.getTaskFlow(id).as(Q.uetfParser.singleOpt).getOrElse(
      throw ResourceNotFoundException(s"Task flow for with id: $id not found!")
    )
  }

  def getTaskFlowByStepAttemptId(stepAttemptId: Long): UserExamStepAttemptTaskFlow = db.run { implicit c =>
    Q.getTaskFlowByStepAttemptId(stepAttemptId).as(Q.uetfParser.singleOpt).getOrElse(
      throw ResourceNotFoundException(s"Task flow for step attempt id: $stepAttemptId not found!")
    )
  }

  def updateTaskFlow(taskFlow: UserExamStepAttemptTaskFlow): UserExamStepAttemptTaskFlow = db.run { implicit c =>
    val affectedRows = Q.updateTaskFlow(taskFlow).executeUpdate()
    if(affectedRows != 1) {
      throw new RuntimeException(s"Failed to update $taskFlow")
    }
    getTaskFlow(taskFlow.id)
  }

  def updateTaskFlowCurrentStep(taskFlowId: Long): UserExamStepAttemptTaskFlow = {
    val taskFlow = getTaskFlow(taskFlowId)
    val steps = findTaskFlowSteps(taskFlowId)
    val nextStep = steps.find(_.sequence == taskFlow.currentStepSequence + 1)
    nextStep.fold(taskFlow)( ns =>
      updateTaskFlow(taskFlow.copy(currentStepSequence = ns.sequence))
    )
  }

  //====================UserExamStepAttemptTaskFlowStep====================

  def createTaskFlowStep(taskFlowStep: UserExamStepAttemptTaskFlowStep): UserExamStepAttemptTaskFlowStep = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTaskFlowStep(taskFlowStep).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $taskFlowStep")
    )
    getTaskFlowStepById(insertedId)
  }

  def getTaskFlowStepById(id: Long): UserExamStepAttemptTaskFlowStep = db.run { implicit c =>
    Q.getTaskFlowStep(id).as(Q.uetfsParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Task flow step with id: $id not found!")
    )
  }

  def getTaskFlowStepBySequence(taskFlowId: Long, sequence: Int): UserExamStepAttemptTaskFlowStep = db.run { implicit c =>
    Q.getTaskFlowStepBySequence(taskFlowId, sequence).as(Q.uetfsParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Task flow step with taskFlowId: $taskFlowId sequence: $sequence not found!")
    )
  }

  def findTaskFlowSteps(stepAttemptTaskFlowId: Long): Seq[UserExamStepAttemptTaskFlowStep] = db.run { implicit c =>
    Q.findTaskFlowSteps(stepAttemptTaskFlowId).as(Q.uetfsParser.*)
  }

  def updateTaskFlowStep(taskFlowStep: UserExamStepAttemptTaskFlowStep): UserExamStepAttemptTaskFlowStep = db.run { implicit c =>
    val affectedRows = Q.updateTaskFlowStep(taskFlowStep).executeUpdate()
    if(affectedRows != 1) {
      throw new RuntimeException(s"Failed to update $taskFlowStep")
    }
    getTaskFlowStepById(taskFlowStep.id)
  }

  def getNotCompletedTaskFlowSteps(stepAttemptId: Long): Seq[UserExamStepAttemptTaskFlowStep] = {
    val taskFlow = getTaskFlowByStepAttemptId(stepAttemptId)
    findTaskFlowSteps(taskFlow.id).filter(!_.done)
  }

  def getTaskFlowDto(stepAttemptId: Long): Option[TaskFlowDto] = {
    val taskFlow = getTaskFlowByStepAttemptId(stepAttemptId)
    val problemVariantConf = problemService.getProblemVariantConfById(taskFlow.problemVariantConfId)
    val problemConf = problemService.getProblemConfById(problemVariantConf.problemConfId)
    val pvcDto = PublicProblemVariantConf(
      problemVariantConf.id,
      problemVariantConf.problemConfId,
      problemVariantConf.schemaType,
      problemVariantConf.schemaUrl,
      problemVariantConf.inputVariableValues
    )
    Some(TaskFlowDto(problemConf, pvcDto, taskFlow))
  }

  def getCurrentTaskFlowStep(taskFlowId: Long): Option[TaskFlowStepDto] = {
    val taskFlow = getTaskFlow(taskFlowId)
    getTaskFlowStep(taskFlow, taskFlow.currentStepSequence)
  }

  def getHelpStepsUpToSequence(taskFlowId: Long, taskFlowStepSequence: Int): Seq[HelpStepDataDto] = db.run{ implicit c =>
    Q.getHelpStepsUpToSequence(taskFlowId, taskFlowStepSequence).as(Q.helpStepsUpToSequenceParser.*)
  }

  def getTaskFlowResultInfoSteps(taskFlowId: Long): Seq[TaskFlowResultInfoStepDataDto] = db.run{ implicit c =>
    Q.getTaskFlowResultInfoSteps(taskFlowId).as(Q.taskFlowResultInfoStepsParser.*)
  }

  def getTaskFlowStep(taskFlow: UserExamStepAttemptTaskFlow, taskFlowStepSequence: Int): Option[TaskFlowStepDto] = {
    val taskFlowStep = getTaskFlowStepBySequence(taskFlow.id, taskFlowStepSequence)
    val taskFlowStepConf = getTaskFlowStepConf(taskFlowStep.taskFlowStepConfId)
    val problemVariantConf = problemService.getProblemVariantConfById(taskFlow.problemVariantConfId)
    val cd = problemVariantConf.calculatedData
    if(taskFlowStepConf.isHelpStep || taskFlowStepConf.isResultInfoStep) {
      updateTaskFlowStep(taskFlowStep.copy(done = true))
      updateTaskFlowCurrentStep(taskFlow.id)
      getTaskFlowStep(taskFlow, taskFlowStepSequence + 1)
    } else {
      val availableHelpSteps = getHelpStepsUpToSequence(taskFlow.id, taskFlowStepSequence)
      val taskFlowStepData: String = taskFlowStepConf.stepType match {
        case TaskFlowStepType.Test =>
          val taskFlowTest = decode[TaskFlowTestConf](taskFlowStepConf.stepData).fold(_=>None,Some(_)).getOrElse(
            throw new RuntimeException(s"Failed to parse test in $taskFlowStepConf")
          )
          val test: TestConf = if (taskFlowTest.test.testType == TestType.SingleInput) {
            taskFlowTest.test.copy(options = taskFlowTest.test.options.map(_.copy(value = ""))) //Single input option values shouldn't be passed to the UI
          } else {
            taskFlowTest.test
          }
          test.asJson.toString()
        case TaskFlowStepType.Charts =>
          taskFlowStep.answer
        case TaskFlowStepType.VariableValueSet =>
          taskFlowStep.answer
        case TaskFlowStepType.DynamicTable =>
          taskFlowStep.answer
        case TaskFlowStepType.EquationSet =>
          val eqSystem = decode[InputSetEquationSystem](taskFlowStepConf.stepData).fold(_=>None,Some(_)).getOrElse(
            throw new RuntimeException(s"Failed to parse InputSetEquationSystem in $taskFlowStepConf")
          )
          val withMappedValues = eqSystem.copy(equations = eqSystem.equations.map(eq =>
            eq.copy(
              items = eq.items.map(i => i.copy(value = i.value.setValue(cd)))
            )
          ))
          withMappedValues.asJson.toString()
        case TaskFlowStepType.Finished =>
          updateTaskFlowStep(taskFlowStep.copy(done = true))
          "Task flow has been finished successfully".asJson.toString()
        case _ => taskFlowStepConf.stepData
      }
      Some(TaskFlowStepDto(taskFlowStepConf, taskFlowStep, taskFlowStepData, availableHelpSteps))
    }
  }

  def verifyTaskFlowStepAnswer(taskFlowStepId: Long, answer: String): Option[VerifiedTaskFlowStepAnswer] = {
    val taskFlowStep = getTaskFlowStepById(taskFlowStepId)
    val taskFlow = getTaskFlow(taskFlowStep.stepAttemptTaskFlowId)
    val taskFlowStepConf = getTaskFlowStepConf(taskFlowStep.taskFlowStepConfId)
    taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val stepTestConf = decode[TaskFlowTestConf](taskFlowStepConf.stepData).fold(_=>None,Some(_)).getOrElse(
          throw new RuntimeException(s"Failed to parse TaskFlowTestConf from step data ${taskFlowStepConf.stepData}")
        )
        val verifiedAnswer = stepTestConf.test.testType match {
          case TestType.SingleInput =>
            val submittedAnswer = decode[TestSingleInputSubmittedAnswer](answer).fold(_=>None,Some(_)).getOrElse(
              throw new RuntimeException(s"Failed to parse test answer in $answer")
            )
            val correctOption = decode[TestOptionConf](taskFlowStep.answer).fold(_=>None,Some(_)).getOrElse(
              throw new RuntimeException(s"Failed to parse correct option in $answer")
            )
            TestUtils.verifySingleInputTest(stepTestConf.test.id, submittedAnswer.submittedAnswer, correctOption, stepTestConf.test.precision)
          case TestType.Radio | TestType.Checkbox =>
            val submittedAnswer = decode[TestSubmittedAnswerDto](answer).fold(_=>None,Some(_)).getOrElse(
              throw new RuntimeException(s"Failed to parse test answer in $answer")
            )
            val correctAnswer = decode[Seq[Long]](taskFlowStep.answer).fold(_=>None,Some(_)).getOrElse(
              throw new RuntimeException(s"Failed to parse correct answer in $answer")
            )
            TestUtils.verifyTraditionalTest(submittedAnswer, correctAnswer)
          case _ =>
            throw new IllegalArgumentException(s"Unsupported test type ${stepTestConf.test.testType} while verifying task flow step answer")
        }
        updateTaskFlowStep(taskFlowStep, verifiedAnswer.isCorrectAnswer, verifiedAnswer.mistakesAmount)
        if(verifiedAnswer.isCorrectAnswer) updateTaskFlowCurrentStep(taskFlow.id)
        Some(
          VerifiedTaskFlowStepAnswer(
            verifiedAnswer.isCorrectAnswer,
            verifiedAnswer.mistakesAmount,
            verifiedAnswer.answer.asJson.toString()
          )
        )
      case TaskFlowStepType.InputSet | TaskFlowStepType.EquationSet =>
        val inputSetAnswer = decode[InputSetAnswerDto](answer).fold(_=>None,Some(_)).getOrElse(
          throw new RuntimeException(s"Failed to parse data in $taskFlowStepConf")
        )
        val correctAnswer = decode[Seq[InputSetInputAnswer]](taskFlowStep.answer).fold(_=>None,Some(_)).getOrElse(
          throw new RuntimeException(s"Failed to parse test answer in $answer")
        )
        val verifiedAnswer = InputSetUtils.verify(inputSetAnswer, correctAnswer, taskFlowStepConf.precision)
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

  def getAvailableProblemVariantConf(problemConfId: Long, examConfId: Long): ProblemVariantConf = db.run { implicit c =>
    val allProblemVariantConfs = problemService.findProblemVariantConfsByProblemConfId(problemConfId)
    val allTaskFlows = Q.findTaskFlowsByExamConfId(examConfId).as(Q.uetfParser.*)
    val usedVariantConfIds = allTaskFlows.map(_.problemVariantConfId)
    val unusedVariantConfs = allProblemVariantConfs.filterNot(vc => usedVariantConfIds.contains(vc.id))
    if(unusedVariantConfs.nonEmpty) {
      scala.util.Random.shuffle(unusedVariantConfs).head
    } else {
      scala.util.Random.shuffle(allProblemVariantConfs).head
    }
  }

  def createTaskFlowWithSteps(userExamStepAttemptId: Long,
                              userExamId: Long,
                              examConfId: Long,
                              examStepConfId: Long,
                              taskFlowConfId: Long,
                              problemConfId: Long): (UserExamStepAttemptTaskFlow, Seq[UserExamStepAttemptTaskFlowStep]) = {
    val problemVariantConf = getAvailableProblemVariantConf(problemConfId, examConfId)
    val cd = problemVariantConf.calculatedData
    val stepConfs = findTaskFlowStepConfs(taskFlowConfId)

    val taskFlow = createTaskFlow(
      UserExamStepAttemptTaskFlow(
        -1,
        userExamStepAttemptId,
        taskFlowConfId,
        problemVariantConf.id,
        1
      )
    )

    val taskFlowSteps = stepConfs.map{ sc =>
      val stepAnswer: String = sc.stepType match {
        case TaskFlowStepType.Test =>
          val stepTestConf = decode[TaskFlowTestConf](sc.stepData).fold(_=>None,Some(_)).getOrElse(
            throw new RuntimeException(s"Failed to parse TaskFlowTestConf from step data ${sc.stepData}")
          )
          stepTestConf.test.testType match {
            case TestType.SingleInput =>
              val correctOption = stepTestConf.test.options.find(_.correct).getOrElse(
                throw new IllegalArgumentException("SingleInput test has to have at least one option")
              )
              correctOption.asJson.toString()
            case TestType.Radio | TestType.Checkbox =>
              stepTestConf.correctOptionIdsMapping
                .fold(stepTestConf.test.getCorrectOptionIds)(mapping =>
                  cd.getString(mapping).split(",").map(_.toLong)
                ).asJson.toString()
            case _ =>
              throw new IllegalArgumentException(s"Unsupported test type ${stepTestConf.test.testType} while saving task flow step")
          }
        case TaskFlowStepType.InputSet =>
          val stepInputSet = decode[InputSet](sc.stepData).fold( e =>
            throw new RuntimeException(s"Failed to parse InputSet from step data ${sc.stepData}", e),
            r => r
          )
          stepInputSet.inputs.map(input =>
            InputSetInputAnswer(input.id, cd.getDoubleOpt(input.answerMapping))
          ).asJson.toString()
        case TaskFlowStepType.VariableValueSet =>
          val stepInputSet = decode[InputSet](sc.stepData).fold( e =>
            throw new RuntimeException(s"Failed to parse InputSet from step data ${sc.stepData}", e),
            r => r
          )
          val inputs = stepInputSet.inputs.map(input =>
            input.copy(value = cd.getDoubleOpt(input.answerMapping))
          )
          stepInputSet.copy(inputs = inputs).asJson.toString()
        case TaskFlowStepType.EquationSet =>
          val eqSystem = decode[InputSetEquationSystem](sc.stepData).fold( e =>
            throw new RuntimeException(s"Failed to parse InputSetEquationSystem from step data ${sc.stepData}", e),
            r => r
          )
          def onlyInput(i: EquationItem) = i.value match {
            case a: SmartValueInput => true
            case _ => false
          }
          val inputs = eqSystem.equations.flatMap(e =>
            e.items.filter(onlyInput).map(i => i.value.asInstanceOf[SmartValueInput])
          )
          inputs.map(i => InputSetInputAnswer(i.id, Some(cd.getDouble(i.answerMapping)))).asJson.toString()
        case TaskFlowStepType.Charts =>
          val decoded = decode[String](sc.stepData).fold(e =>
            throw new RuntimeException(s"Failed to parse string from step data ${sc.stepData}", e),
            r => r
          )
          cd.get(decoded) match {
            case chartSet: ChartSet => chartSet.asJson.toString()
            case _ => throw new RuntimeException(s"Failed to get ChartSet with key ${sc.stepData}")
          }
        case TaskFlowStepType.DynamicTable =>
          if(sc.stepData.matches("\"\\{.+\\}\"")) {
            val decoded = decode[DynamicTable](sc.stepData).fold(e =>
              throw new RuntimeException(s"Failed to parse DynamicTable from step data ${sc.stepData}", e),
              r => r
            )
            decoded.copy(
              rows = decoded.rows.map(r => r.copy(
                cells = r.cells.map(c => c.setValue(cd)))
              )
            ).asJson.toString()
          } else if(sc.stepData.matches("\"[\\w-_]+\"")) {
            val decoded = decode[String](sc.stepData).fold(e =>
              throw new RuntimeException(s"Failed to parse DynamicTable mapping string from step data ${sc.stepData}", e),
              r => r
            )
            cd.get(decoded) match {
              case table: DynamicTable => table.asJson.toString()
              case _ => throw new RuntimeException(s"Failed to get DynamicTable with key ${sc.stepData}")
            }
          } else {
            throw new RuntimeException(s"Unknown DynamicTable step data ${sc.stepData}")
          }
        case TaskFlowStepType.Finished =>
          "".asJson.toString()
        case st => throw new RuntimeException(s"Unhandled step type: $st")
      }
      createTaskFlowStep(UserExamStepAttemptTaskFlowStep(-1, taskFlow.id, sc.id, sc.sequence, stepAnswer, sc.isResultInfoStep))
    }

    (taskFlow, taskFlowSteps)
  }
}

object TaskFlowQueries {
  import anorm.SqlParser.{int, long, str, bool, double}

  object TFC {
    val table = "task_flow_confs"
    val id = "id"
    val problemConfId = "problem_conf_id"
    val name = "name"
  }

  object TFSC {
    val table = "task_flow_step_confs"
    val id = "id"
    val taskFlowConfId = "task_flow_conf_id"
    val name = "name"
    val sequence = "sequence"
    val stepType = "step_type"
    val stepData = "step_data"
    val precision = "calculation_precision"
    val isHelpStep = "is_help_step"
    val isResultInfoStep = "is_result_info_step"
  }

  object UETF {
    val table = "user_exam_task_flows"
    val id = "id"
    val stepAttemptId = "step_attempt_id"
    val taskFlowConfId = "task_flow_conf_id"
    val problemVariantConfId = "problem_variant_conf_id"
    val currentStepSequence = "current_step_sequence"
  }

  object UETFS {
    val table = "user_exam_task_flow_steps"
    val id = "id"
    val stepAttemptTaskFlowId = "step_attempt_task_flow_id"
    val taskFlowStepConfId = "task_flow_step_conf_id"
    val sequence = "sequence"
    val answer = "answer"
    val done = "done"
    val mistakes = "mistakes"
  }

  val tfcParser  = for {
    id <- long(TFC.id)
    problemConfId <- long(TFC.problemConfId)
    name <- str(TFC.name)
  } yield TaskFlowConf(id, problemConfId, name)

  val tfscParser  = for {
    id <- long(TFSC.id)
    taskFlowConfId <- long(TFSC.taskFlowConfId)
    name <- str(TFSC.name)
    sequence <- int(TFSC.sequence)
    stepType <- int(TFSC.stepType)
    stepData <- str(TFSC.stepData)
    precision <- double(TFSC.precision).?
    isHelpStep <- bool(TFSC.isHelpStep)
    isResultInfoStep <- bool(TFSC.isResultInfoStep)
  } yield TaskFlowStepConf(id, taskFlowConfId, sequence, name, TaskFlowStepType(stepType), stepData, precision, isHelpStep, isResultInfoStep)

  val uetfParser  = for {
    id <- long(UETF.id)
    stepAttemptId <- long(UETF.stepAttemptId)
    taskFlowConfId <- long(UETF.taskFlowConfId)
    problemVariantConfId <- long(UETF.problemVariantConfId)
    currentStepSequence <- int(UETF.currentStepSequence)
  } yield UserExamStepAttemptTaskFlow(id, stepAttemptId, taskFlowConfId, problemVariantConfId, currentStepSequence)

  val uetfsParser  = for {
    id <- long(UETFS.id)
    stepAttemptTaskFlowId <- long(UETFS.stepAttemptTaskFlowId)
    taskFlowStepConfId <- long(UETFS.taskFlowStepConfId)
    sequence <- int(UETFS.sequence)
    answer <- str(UETFS.answer)
    done <- bool(UETFS.done)
    mistakes <- int(UETFS.mistakes)
  } yield UserExamStepAttemptTaskFlowStep(id, stepAttemptTaskFlowId, taskFlowStepConfId, sequence, answer, done, mistakes)

  val helpStepsUpToSequenceParser  = for {
    id <- long(UETFS.id)
    stepType <- int(TFSC.stepType)
    answer <- str(UETFS.answer)
    name <- str(TFSC.name)
  } yield HelpStepDataDto(id, TaskFlowStepType(stepType), name, answer)

  val taskFlowResultInfoStepsParser  = for {
    id <- long(UETFS.id)
    stepType <- int(TFSC.stepType)
    answer <- str(UETFS.answer)
    name <- str(TFSC.name)
  } yield TaskFlowResultInfoStepDataDto(id, TaskFlowStepType(stepType), name, answer)

  def createTaskFlowConf(tfc: TaskFlowConf) =
    SQL(s"INSERT INTO ${TFC.table} (${TFC.problemConfId}, ${TFC.name}) VALUES ({problemConfId}, {name})")
      .on("problemConfId" -> tfc.problemConfId)
      .on("name" -> tfc.name)

  def updateTaskFlowConf(id: Long, tfc: TaskFlowConf) =
    SQL(s"UPDATE ${TFC.table} SET ${TFC.problemConfId} = {problemConfId}, ${TFC.name} = {name} WHERE ${TFC.id} = {id}")
      .on("id" -> id)
      .on("problemConfId" -> tfc.problemConfId)
      .on("name" -> tfc.name)

  def deleteTaskFlowConf(id: Long) =
    SQL(s"DELTE FROM ${TFC.table} WHERE ${TFC.id} = {id}")
      .on("id" -> id)

  def createTaskFlowStepConf(tfsc: TaskFlowStepConf) =
    SQL(
      s"""INSERT INTO ${TFSC.table} (
         |${TFSC.taskFlowConfId},
         |${TFSC.name},
         |${TFSC.sequence},
         |${TFSC.stepType},
         |${TFSC.stepData},
         |${TFSC.precision},
         |${TFSC.isHelpStep},
         |${TFSC.isResultInfoStep}
         |) VALUES (
         |{taskFlowConfId},
         |{name},
         |{sequence},
         |{stepType},
         |{stepData},
         |{precision},
         |{isHelpStep},
         |{isResultInfoStep}
         |)""".stripMargin)
    .on("taskFlowConfId" -> tfsc.taskFlowConfId)
    .on("name" -> tfsc.name)
    .on("sequence" -> tfsc.sequence)
    .on("stepType" -> tfsc.stepType.id)
    .on("stepData" -> tfsc.stepData)
    .on("precision" -> tfsc.precision)
    .on("isHelpStep" -> tfsc.isHelpStep)
    .on("isResultInfoStep" -> tfsc.isResultInfoStep)

  def updateTaskFlowStepConf(id: Long, tfsc: TaskFlowStepConf) =
    SQL(
      s"""UPDATE ${TFSC.table} SET
         |${TFSC.name} = {name},
         |${TFSC.sequence} = {sequence},
         |${TFSC.stepType} = {stepType},
         |${TFSC.stepData} = {stepData},
         |${TFSC.precision} = {precision},
         |${TFSC.isHelpStep} = {isHelpStep},
         |${TFSC.isResultInfoStep} = {isResultInfoStep}
         |WHERE ${TFSC.id} = {id}""".stripMargin)
      .on("id" -> id)
      .on("name" -> tfsc.name)
      .on("sequence" -> tfsc.sequence)
      .on("stepType" -> tfsc.stepType.id)
      .on("stepData" -> tfsc.stepData)
      .on("precision" -> tfsc.precision)
      .on("isHelpStep" -> tfsc.isHelpStep)
      .on("isResultInfoStep" -> tfsc.isResultInfoStep)

  def deleteTaskFlowStepConfs(ids: Seq[Long]) =
    SQL(s"DELETE FROM ${TFSC.table} WHERE ${TFSC.id} IN (${if (ids.nonEmpty) ids.mkString(", ") else "0"})")

  def createTaskFlow(uetf: UserExamStepAttemptTaskFlow) =
    SQL(
      s"""INSERT INTO ${UETF.table} (
         |${UETF.stepAttemptId},
         |${UETF.taskFlowConfId},
         |${UETF.problemVariantConfId},
         |${UETF.currentStepSequence}
         |) VALUES (
         |{stepAttemptId},
         |{taskFlowConfId},
         |{problemVariantConfId},
         |{currentStepSequence}
         |)""".stripMargin)
      .on("stepAttemptId" -> uetf.stepAttemptId)
      .on("taskFlowConfId" -> uetf.taskFlowConfId)
      .on("problemVariantConfId" -> uetf.problemVariantConfId)
      .on("currentStepSequence" -> uetf.currentStepSequence)

  def updateTaskFlow(uetf: UserExamStepAttemptTaskFlow) =
    SQL(
      s"""UPDATE ${UETF.table} SET
         |${UETF.currentStepSequence}={currentStepSequence}
         |WHERE ${UETF.id} = {id}
         |""".stripMargin)
      .on("id" -> uetf.id)
      .on("currentStepSequence" -> uetf.currentStepSequence)

  def createTaskFlowStep(uetfs: UserExamStepAttemptTaskFlowStep) =
    SQL(
      s"""INSERT INTO ${UETFS.table} (
         |${UETFS.stepAttemptTaskFlowId},
         |${UETFS.taskFlowStepConfId},
         |${UETFS.sequence},
         |${UETFS.answer},
         |${UETFS.done},
         |${UETFS.mistakes}
         |) VALUES (
         |{stepAttemptTaskFlowId},
         |{taskFlowStepConfId},
         |{sequence},
         |{answer},
         |{done},
         |{mistakes}
         |)""".stripMargin)
      .on("stepAttemptTaskFlowId" -> uetfs.stepAttemptTaskFlowId)
      .on("taskFlowStepConfId" -> uetfs.taskFlowStepConfId)
      .on("sequence" -> uetfs.sequence)
      .on("answer" -> uetfs.answer)
      .on("done" -> uetfs.done)
      .on("mistakes" -> uetfs.mistakes)

  def updateTaskFlowStep(uetfs: UserExamStepAttemptTaskFlowStep) =
    SQL(
      s"""UPDATE ${UETFS.table} SET
         |${UETFS.done}={done},
         |${UETFS.mistakes}={mistakes}
         |WHERE ${UETFS.id} = {id}
         |""".stripMargin)
      .on("id" -> uetfs.id)
      .on("done" -> uetfs.done)
      .on("mistakes" -> uetfs.mistakes)

  def findTaskFlowsByExamConfId(examConfId: Long) = SQL(
    s"""
       |SELECT tf.*  FROM user_exam_task_flows tf
       |JOIN user_exam_step_attempts sa ON sa.id = tf.step_attempt_id
       |JOIN user_exams e ON e.id = sa.user_exam_id
       |JOIN exam_confs ec ON ec.id = e.exam_conf_id
       |WHERE e.id = {examConfId}
      """.stripMargin)
    .on("examConfId" -> examConfId)

  def getTaskFlowConf(id: Long) = SqlUtils.get(TFC.table, id)

  def getTaskFlowStepConf(id: Long) = SqlUtils.get(TFSC.table, id)

  def findTaskFlowStepConfs(taskFlowConfId: Long) =
    SQL(s"SELECT * FROM ${TFSC.table} WHERE ${TFSC.taskFlowConfId} = {taskFlowConfId}").on("taskFlowConfId" -> taskFlowConfId)

  def getTaskFlow(id: Long) = SqlUtils.get(UETF.table, id)

  def getTaskFlowByStepAttemptId(stepAttemptId: Long) =
    SQL(s"SELECT * FROM ${UETF.table} WHERE ${UETF.stepAttemptId} = {stepAttemptId}").on("stepAttemptId" -> stepAttemptId)

  def getTaskFlowStep(id: Long) = SqlUtils.get(UETFS.table, id)

  def getTaskFlowStepBySequence(stepAttemptTaskFlowId: Long, sequence: Int) =
    SQL(s"SELECT * FROM ${UETFS.table} WHERE ${UETFS.sequence} = {sequence} AND ${UETFS.stepAttemptTaskFlowId} = {stepAttemptTaskFlowId}")
    .on("sequence" -> sequence)
    .on("stepAttemptTaskFlowId" -> stepAttemptTaskFlowId)

  def getHelpStepsUpToSequence(stepAttemptTaskFlowId: Long, sequence: Int) =
    SQL(
      s"""SELECT s.id, s.${UETFS.answer}, sc.${TFSC.stepType}, sc.${TFSC.name} FROM ${UETFS.table} s
         |INNER JOIN ${TFSC.table} sc ON sc.id = s.${UETFS.taskFlowStepConfId}
         |WHERE sc.${TFSC.isHelpStep} IS TRUE
         |AND s.${UETFS.sequence} <= {sequence}
         |AND s.${UETFS.stepAttemptTaskFlowId} = {stepAttemptTaskFlowId}""".stripMargin)
      .on("sequence" -> sequence)
      .on("stepAttemptTaskFlowId" -> stepAttemptTaskFlowId)

  def getTaskFlowResultInfoSteps(stepAttemptTaskFlowId: Long) =
    SQL(
      s"""SELECT s.id, s.${UETFS.answer}, sc.${TFSC.stepType}, sc.${TFSC.name} FROM ${UETFS.table} s
         |INNER JOIN ${TFSC.table} sc ON sc.id = s.${UETFS.taskFlowStepConfId}
         |WHERE sc.${TFSC.isResultInfoStep} IS TRUE
         |AND s.${UETFS.stepAttemptTaskFlowId} = {stepAttemptTaskFlowId}""".stripMargin)
      .on("stepAttemptTaskFlowId" -> stepAttemptTaskFlowId)

  def findTaskFlowSteps(stepAttemptTaskFlowId: Long) =
    SQL(s"SELECT * FROM ${UETFS.table} WHERE ${UETFS.stepAttemptTaskFlowId} = {stepAttemptTaskFlowId}")
      .on("stepAttemptTaskFlowId" -> stepAttemptTaskFlowId)
}
