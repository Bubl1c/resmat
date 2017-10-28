package edu.knuca.resmat.exam

import edu.knuca.resmat.core.{ProblemAnswer, RingPlateProblemAnswer}
import edu.knuca.resmat.utils.PimpedEnumeration
import io.circe.generic.JsonCodec
import org.joda.time.DateTime

object TaskFlowStepType extends PimpedEnumeration {
  type TaskFlowStepType = Value
  val Test = Value(1, "test")
  val InputSet = Value(2, "input-set")
  val Charts = Value(3, "charts")
  val VariableValueSet = Value(4, "var-value-set")
  val EquationSet = Value(5, "equation-set")
  val Finished = Value(-1, "finished")
}

object ExamStepType extends PimpedEnumeration {
  type ExamStepType = Value
  val TestSet = Value(1, "test-set")
  val TaskFlow = Value(2, "task-flow")
  val Results = Value(3, "results")
}

object ProblemType extends PimpedEnumeration {
  type ProblemType = Value
  val RingPlate = Value(1, "ring-plate")
}

object TestType extends PimpedEnumeration {
  type TestType = Value
  val Radio = Value(1, "radio")
  val Checkbox = Value(2, "checkbox")
}

object ExamStatus extends PimpedEnumeration {
  type ExamStatus = Value
  val Initial = Value(1, "initial")
  val InProgress = Value(2, "in-progress")
  val Failed = Value(3, "failed")
  val Success = Value(4, "success")
}

object ExamStepStatus extends PimpedEnumeration {
  type ExamStepStatus = Value
  val NotSubmitted = Value(1, "not-submitted")
  val Failed = Value(2, "failed")
  val Success = Value(3, "success")
}

object TestOptionValueType extends PimpedEnumeration {
  type TestOptionValueType = Value
  val Image = Value(1, "img")
  val Text = Value(2, "words")
}

case class ExamConf(id: Long, name: String, description: String, maxScore: Int)
case class ExamStepConf(id: Long,
                        examConfId: Long,
                        sequence: Int,
                        name: String,
                        stepType: ExamStepType.ExamStepType,
                        mistakesPerAttemptLimit: Int,
                        mistakeValuePercents: Int, //influence to result
                        attemptsLimit: Int,
                        attemptValuePercents: Int, //influence to result
                        maxScore: Int, //should be within ExamConf.maxScore
                        dataSet: ExamStepConfDataSet,
                        hasToBeSubmitted: Boolean = true)
@JsonCodec sealed trait ExamStepConfDataSet //to make JsonCodec work
case class ExamStepTestSetDataSet(testSetConfId: Long) extends ExamStepConfDataSet
case class ExamStepTaskFlowDataSet(taskFlowConfId: Long, problemConfId: Long) extends ExamStepConfDataSet
case object ExamStepResultsDataSet extends ExamStepConfDataSet
object ExamStepConfDataSet

case class UserExam(id: Long,
                    userId: Long,
                    examConfId: Long,
                    currentStepConfId: Long,
                    status: ExamStatus.ExamStatus,
                    lockedUntil: Option[DateTime],
                    started: Option[DateTime],
                    finished: Option[DateTime])
case class UserExamStepAttempt(id: Long,
                               userExamId: Long,
                               examStepConfId: Long,
                               mistakesAmount: Int,
                               attemptNumber: Int/*Starts with 1*/,
                               status: ExamStepStatus.ExamStepStatus)
case class UserExamResult(id: Long,
                          userExamId: Long,
                          examConfId: Long,
                          userId: Long,
                          examName: String,
                          studentName: String,
                          studentGroupName: Option[String],
                          durationMillis: Long,
                          stepResults: Seq[UserExamStepResult],
                          score: Int,
                          maxScore: Int) extends StepDataDto
case class UserExamStepResult(stepConfId: Long,
                              sequence: Int,
                              name: String,
                              attemptsAmount: Int,
                              mistakesAmount: Int,
                              durationMillis: Long)

//====================TestSet====================

case class TestSetConf(id: Long, name: String, maxTestsAmount: Int)
case class TestSetConfTestGroup(id: Long, testSetConfId: Long, testGroupConfId: Long, proportionPercents: Int)

case class TestGroupConf(id: Long, name: String)
case class TestConf(id: Long,
                    groupId: Long,
                    question: String,
                    imageUrl: Option[String],
                    options: Seq[TestOptionConf],
                    testType: TestType.TestType = TestType.Radio,
                    help: Option[String] = None) {
  def getCorrectOptionIds: Seq[Long] = options.filter(_.correct).map(_.id)
  def normalised: TestConf = {
    val optionsWithNormalisedIds = options.zipWithIndex.map{ case(opt, i) => opt.copy(id = i) }
    this.copy(options = optionsWithNormalisedIds)
  }
}
case class TestOptionConf(id: Long,
                          value: String,
                          correct: Boolean = false,
                          valueType: TestOptionValueType.TestOptionValueType = TestOptionValueType.Text)

case class UserExamStepAttemptTestSet(id: Long, stepAttemptId: Long, testSetConfId: Long)
case class UserExamStepAttemptTestSetTest(id: Long,
                                          stepAttemptTestSetId: Long,
                                          testConfId: Long,
                                          done: Boolean = false,
                                          mistakes: Int = 0)

//====================Problem====================

case class ProblemConf(id: Long, name: String, problemType: ProblemType.ProblemType, inputVariableConfs: Seq[ProblemInputVariableConf])
case class ProblemInputVariableConf(id: Int, name: String, units: String = "", alias: String, showInExam: Boolean = true)
case class ProblemInputVariableValue(variableConfId: Long, value: Double)
//todo switch calculatedData to interface to allow to work with different problems
case class ProblemVariantConf(id: Long,
                              problemConfId: Long,
                              schemaUrl: String,
                              inputVariableValues: Seq[ProblemInputVariableValue],
                              calculatedData: RingPlateProblemAnswer)

//====================TaskFlow====================

case class TaskFlowConf(id: Long, problemConfId: Long, name: String)
case class TaskFlowStepConf(id: Long,
                            taskFlowConfId: Long,
                            sequence: Int,
                            name: String,
                            stepType: TaskFlowStepType.TaskFlowStepType,
                            stepData: String /*JSON field*/,
                            precision: Option[Double] = None,
                            isHelpStep: Boolean = false)

case class UserExamStepAttemptTaskFlow(id: Long,
                                       stepAttemptId: Long,
                                       taskFlowConfId: Long,
                                       problemVariantConfId: Long,
                                       currentStepSequence: Int)
case class UserExamStepAttemptTaskFlowStep(id: Long,
                                           stepAttemptTaskFlowId: Long,
                                           taskFlowStepConfId: Long,
                                           sequence: Int,
                                           answer: String/*JSON object*/,
                                           done: Boolean = false,
                                           mistakes: Int = 0)

sealed trait TaskFlowStepData

case class InputSet(id: Long, name: String, inputs: Seq[InputSetInput]) extends TaskFlowStepData {
  def normalised: InputSet = {
    val inputsWithNormalisedIds = inputs.zipWithIndex.map{ case (i, ind) => i.copy(id = ind + 1)}
    this.copy(inputs = inputsWithNormalisedIds)
  }
}
case class InputSetInput(id: Int, //unique within input set
                         name: String,
                         groupName: String,
                         units: String,
                         answerMapping: String,
                         description: String = "",
                         value: Option[Double] = None)
@JsonCodec sealed trait EquationItemValue {
  def setValue(answer: ProblemAnswer): EquationItemValue = this
}
case class EquationItemValueInput(id: Int, answerMapping: String) extends EquationItemValue
case class EquationItemValueDynamicDouble(answerMapping: String, value: Double = 0.0, digitsAfterComma: Int = 6) extends EquationItemValue {
  override def setValue(answer: ProblemAnswer): EquationItemValue =
    this.copy(value = answer.getDouble(answerMapping))
}
case class EquationItemValueStaticString(value: String) extends EquationItemValue

object EquationItemValue //to make JsonCodec work

case class EquationItem(value: EquationItemValue, prefix: String = "", suffix: String = "")
case class InputSetEquation(id: Int, items: List[EquationItem])
case class InputSetEquationSystem(name: String, equations: List[InputSetEquation])

case class TaskFlowTestConf(test: TestConf, correctOptionIdsMapping: Option[String] = None) extends TaskFlowStepData

case class ChartData(title: String, x: Array[Double], y: Array[Double], bottom: Boolean = false, positive: Boolean = true) extends TaskFlowStepData
case class ChartSet(title: String, charts: Seq[ChartData])
