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
                       taskFlow: UserExamStepAttemptTaskFlow)
case class TaskFlowStepDto(taskFlowStepConf: TaskFlowStepConf,
                           stepAttemptTaskFlowStep: UserExamStepAttemptTaskFlowStep,
                           taskFlowStepData: String)

class TaskFlowExamService(val db: DatabaseService)
                         (testSetExamService: TestSetExamService)
                         (implicit val executionContext: ExecutionContext) extends LazyLogging {

  val problemConfs: List[ProblemConf] = List()
  val problemVariantConfs: List[ProblemVariantConf] = List()
  val calculatedProblemVariantConfs: List[CalculatedProblemVariantConf] = List()

  val taskFlowConfs: List[TaskFlowConf] = List()
  val taskFlowStepConfs: List[TaskFlowStepConf] = List()

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
    val problemVariantConf = problemVariantConfs.find(_.id == taskFlowConf.problemVariantConfId).getOrElse(
      throw new RuntimeException(s"Problem variant conf with id: ${taskFlowConf.problemVariantConfId} not found!")
    )
    val problemConf = problemConfs.find(_.id == problemVariantConf.problemConfId).getOrElse(
      throw new RuntimeException(s"Problem conf with id: ${problemVariantConf.problemConfId} not found!")
    )
    Some(TaskFlowDto(taskFlowConf, problemConf, problemVariantConf, taskFlow))
  }

  def getTaskFlowStep(taskFlowId: Long, stepSequence: Int): Option[TaskFlowStepDto] = {
    val taskFlow = stepAttemptTaskFlows.find(_.id == taskFlowId).getOrElse(
      throw new IllegalArgumentException(s"Task flow with id: $taskFlowId not found!")
    )
    val taskFlowConf = taskFlowConfs.find(_.id == taskFlow.taskFlowConfId).getOrElse(
      throw new RuntimeException(s"Task flow conf with id: ${taskFlow.taskFlowConfId} not found! For task flow: $taskFlow")
    )
    val taskFlowStepConf = taskFlowStepConfs.find(sc => sc.taskFlowConfId == taskFlowConf.id && sc.sequence == stepSequence).getOrElse(
      throw new RuntimeException(s"Task flow step conf with sequence $stepSequence and taskFlowConfId: ${taskFlowConf.id} not found!")
    )
    val taskFlowStep = stepAttemptTaskFlowSteps.find(s => s.taskFlowStepConfId == taskFlowStepConf.id && s.stepAttemptTaskFlowId == taskFlow.id).getOrElse(
      throw new RuntimeException(s"Task flow step with taskFlowStepConfId: ${taskFlowStepConf.id} and stepAttemptTaskFlowId: ${taskFlow.id} not found!")
    )
    val taskFlowStepData: String = taskFlowStepConf.stepType match {
      case TaskFlowStepType.Test =>
        val taskFlowTest = decode[TaskFlowTest](taskFlowStepConf.stepData).fold(er => None, test => Some(test)).getOrElse(
          throw new RuntimeException(s"Failed to parse data in $taskFlowStepConf")
        )
        val testConf = testSetExamService.getTestConfs(Seq(taskFlowTest.testId)).headOption.getOrElse(
          throw new RuntimeException(s"Test with id: ${taskFlowTest.testId}")
        )
        testConf.asJson.toString()
      case TaskFlowStepType.InputSet =>
        val inputSet = decode[InputSet](taskFlowStepConf.stepData).fold(er => None, is => Some(is)).getOrElse(
          throw new RuntimeException(s"Failed to parse data in $taskFlowStepConf")
        )
        inputSet.asJson.toString()
      case TaskFlowStepType.Charts =>
        "not implemented"
    }
    Some(TaskFlowStepDto(taskFlowStepConf, taskFlowStep, taskFlowStepData))
  }
}
