package edu.knuca.resmat.exam.taskflow

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.core._
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.testset.{TestAnswerDto, TestUtils}
import edu.knuca.resmat.utils.SqlUtils
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

case class TaskFlowDto(problemConf: ProblemConf,
                       problemVariantConf: ProblemVariantConfDto,
                       taskFlow: UserExamStepAttemptTaskFlow) extends StepDataDto
case class TaskFlowStepDto(taskFlowStepConf: TaskFlowStepConf,
                           stepAttemptTaskFlowStep: UserExamStepAttemptTaskFlowStep,
                           taskFlowStepData: String)

case class ProblemVariantConfDto(id: Long,
                                 problemConfId: Long,
                                 schemaUrl: String,
                                 inputVariableValues: Seq[ProblemInputVariableValue])

case class InputSetAnswerDto(inputSetId: Long, inputAnswers: Seq[InputSetInputAnswer])
case class InputSetInputAnswer(id: Int, value: Option[Double] = None)

case class VerifiedTaskFlowStepAnswer(isCorrectAnswer: Boolean, mistakesAmount: Int, answer: String)

case class VerifiedInputSetAnswer(inputSetId: Long, isCorrectAnswer: Boolean, mistakesAmount: Int, answer: Map[Int, Boolean])

case class ResourceNotFoundException(message: String) extends IllegalArgumentException {
  override def getMessage: String = super.getMessage + " " + message
}

class TaskFlowExamService(val db: DatabaseService)
                         (val problemService: ProblemService)
                         (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.exam.taskflow.{TaskFlowQueries => Q}

  import edu.knuca.resmat.http.JsonProtocol._

  //====================TaskFlowConf====================

  def createTaskFlowConf(taskFlowConf: TaskFlowConf): TaskFlowConf = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTaskFlowConf(taskFlowConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $taskFlowConf")
    )
    getTaskFlowConf(insertedId)
  }

  def getTaskFlowConf(id: Long): TaskFlowConf = db.run { implicit c =>
    Q.getTaskFlowConf(id).as(Q.tfParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Task flow step conf with id: $id not found!")
    )
  }

  //====================TaskFlowStepConf====================

  def createTaskFlowStepConf(taskFlowStepConf: TaskFlowStepConf): TaskFlowStepConf = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTaskFlowStepConf(taskFlowStepConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $taskFlowStepConf")
    )
    getTaskFlowStepConf(insertedId)
  }

  def getTaskFlowStepConf(id: Long): TaskFlowStepConf = db.run { implicit c =>
    Q.getTaskFlowStepConf(id).as(Q.tfsParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Task flow step conf with id: $id not found!")
    )
  }

  def findTaskFlowStepConfs(taskFlowConfId: Long): Seq[TaskFlowStepConf] = db.run { implicit c =>
    Q.findTaskFlowStepConfs(taskFlowConfId).as(Q.tfsParser.*)
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
    Some(TaskFlowDto(problemConf, problemVariantConf, taskFlow))
  }

  def getCurrentTaskFlowStep(taskFlowId: Long): Option[TaskFlowStepDto] = {
    val taskFlow = getTaskFlow(taskFlowId)
    getTaskFlowStep(taskFlow.id, taskFlow.currentStepSequence)
  }

  def getTaskFlowStep(taskFlowId: Long, taskFlowStepSequence: Int): Option[TaskFlowStepDto] = {
    val taskFlowStep = getTaskFlowStepBySequence(taskFlowId, taskFlowStepSequence)
    val taskFlowStepConf = getTaskFlowStepConf(taskFlowStep.taskFlowStepConfId)
    if(taskFlowStepConf.isHelpStep) {
      updateTaskFlowStep(taskFlowStep.copy(done = true))
      updateTaskFlowCurrentStep(taskFlowId)
    }
    val taskFlowStepData: String = taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val taskFlowTest = decode[TaskFlowTestConf](taskFlowStepConf.stepData).fold(_=>None,Some(_)).getOrElse(
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
    val taskFlow = getTaskFlow(taskFlowStep.stepAttemptTaskFlowId)
    val taskFlowStepConf = getTaskFlowStepConf(taskFlowStep.taskFlowStepConfId)
    taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val testAnswer = decode[TestAnswerDto](answer).fold(_=>None,Some(_)).getOrElse(
          throw new RuntimeException(s"Failed to parse test answer in $answer")
        )
        val correctAnswer = decode[Seq[Long]](taskFlowStep.answer).fold(_=>None,Some(_)).getOrElse(
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
        val inputSetAnswer = decode[InputSetAnswerDto](answer).fold(_=>None,Some(_)).getOrElse(
          throw new RuntimeException(s"Failed to parse data in $taskFlowStepConf")
        )
        val correctAnswer = decode[Seq[InputSetInputAnswer]](taskFlowStep.answer).fold(_=>None,Some(_)).getOrElse(
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

  def createTaskFlowWithSteps(stepAttemptId: Long,
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
        stepAttemptId,
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
          stepTestConf.correctOptionIdsMapping
            .fold(stepTestConf.test.getCorrectOptionIds)(mapping =>
              cd.getString(mapping).split(",").map(_.toLong)
            ).asJson.toString()
        case TaskFlowStepType.InputSet =>
          val stepInputSet = decode[InputSet](sc.stepData).fold( e =>
            throw new RuntimeException(s"Failed to parse InputSet from step data ${sc.stepData}", e),
            r => r
          )
          stepInputSet.inputs.map(input =>
            InputSetInputAnswer(input.id, cd.getDoubleOpt(input.answerMapping))
          ).asJson.toString()
        case TaskFlowStepType.Charts =>
          val decoded = decode[String](sc.stepData).fold(e =>
            throw new RuntimeException(s"Failed to parse string from step data ${sc.stepData}", e),
            r => r
          )
          cd.get(decoded) match {
            case chartSet: ChartSet => chartSet.asJson.toString()
            case _ => throw new RuntimeException(s"Failed to get ChartSet with key ${sc.stepData}")
          }
        case TaskFlowStepType.Finished =>
          "".asJson.toString()
        case st => throw new RuntimeException(s"Unhandled step type: $st")
      }
      createTaskFlowStep(UserExamStepAttemptTaskFlowStep(-1, taskFlow.id, sc.id, sc.sequence, stepAnswer))
    }

    (taskFlow, taskFlowSteps)
  }
}

object TaskFlowQueries {
  import anorm.SqlParser.{int, long, str, bool}

  object TF {
    val table = "task_flow_confs"
    val id = "id"
    val problemConfId = "problem_conf_id"
    val name = "name"
  }

  object TFS {
    val table = "task_flow_step_confs"
    val id = "id"
    val taskFlowConfId = "task_flow_conf_id"
    val name = "name"
    val sequence = "sequence"
    val stepType = "step_type"
    val stepData = "step_data"
    val isHelpStep = "is_help_step"
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

  val tfParser  = for {
    id <- long(TF.id)
    problemConfId <- long(TF.problemConfId)
    name <- str(TF.name)
  } yield TaskFlowConf(id, problemConfId, name)

  val tfsParser  = for {
    id <- long(TFS.id)
    taskFlowConfId <- long(TFS.taskFlowConfId)
    name <- str(TFS.name)
    sequence <- int(TFS.sequence)
    stepType <- int(TFS.stepType)
    stepData <- str(TFS.stepData)
    isHelpStep <- bool(TFS.isHelpStep)
  } yield TaskFlowStepConf(id, taskFlowConfId, name, sequence, TaskFlowStepType(stepType), stepData, isHelpStep)

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

  def createTaskFlowConf(tf: TaskFlowConf) =
    SQL(s"INSERT INTO ${TF.table} (${TF.problemConfId}, ${TF.name}) VALUES ({problemConfId}, {name})")
      .on("problemConfId" -> tf.problemConfId)
      .on("name" -> tf.name)

  def createTaskFlowStepConf(tfs: TaskFlowStepConf) =
    SQL(
      s"""INSERT INTO ${TFS.table} (
         |${TFS.taskFlowConfId},
         |${TFS.name},
         |${TFS.sequence},
         |${TFS.stepType},
         |${TFS.stepData},
         |${TFS.isHelpStep}
         |) VALUES (
         |{taskFlowConfId},
         |{name},
         |{sequence},
         |{stepType},
         |{stepData},
         |{isHelpStep}
         |)""".stripMargin)
    .on("taskFlowConfId" -> tfs.taskFlowConfId)
    .on("name" -> tfs.name)
    .on("sequence" -> tfs.sequence)
    .on("stepType" -> tfs.stepType.id)
    .on("stepData" -> tfs.stepData)
    .on("isHelpStep" -> tfs.isHelpStep)

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

  def getTaskFlowConf(id: Long) = SqlUtils.get(TF.table, id)

  def getTaskFlowStepConf(id: Long) = SqlUtils.get(TFS.table, id)

  def findTaskFlowStepConfs(taskFlowConfId: Long) =
    SQL(s"SELECT * FROM ${TFS.table} WHERE ${TFS.taskFlowConfId} = {taskFlowConfId}").on("taskFlowConfId" -> taskFlowConfId)

  def getTaskFlow(id: Long) = SqlUtils.get(UETF.table, id)

  def getTaskFlowByStepAttemptId(stepAttemptId: Long) =
    SQL(s"SELECT * FROM ${UETF.table} WHERE ${UETF.stepAttemptId} = {stepAttemptId}").on("stepAttemptId" -> stepAttemptId)

  def getTaskFlowStep(id: Long) = SqlUtils.get(UETFS.table, id)

  def getTaskFlowStepBySequence(stepAttemptTaskFlowId: Long, sequence: Int) =
    SQL(s"SELECT * FROM ${UETFS.table} WHERE ${UETFS.sequence} = {sequence} AND ${UETFS.stepAttemptTaskFlowId} = {stepAttemptTaskFlowId}")
    .on("sequence" -> sequence)
    .on("stepAttemptTaskFlowId" -> stepAttemptTaskFlowId)

  def findTaskFlowSteps(stepAttemptTaskFlowId: Long) =
    SQL(s"SELECT * FROM ${UETFS.table} WHERE ${UETFS.stepAttemptTaskFlowId} = {stepAttemptTaskFlowId}")
      .on("stepAttemptTaskFlowId" -> stepAttemptTaskFlowId)
}
