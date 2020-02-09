package edu.knuca.resmat.exam

import edu.knuca.resmat.core.ProblemAnswer
import edu.knuca.resmat.core.RingPlateProblemAnswer
import edu.knuca.resmat.exam.taskflow.{TaskFlowDto, TaskFlowResultInfoStepDataDto}
import edu.knuca.resmat.utils.PimpedEnumeration
import io.circe.generic.JsonCodec
import org.joda.time.DateTime

//Both needed for UserExamStepResultStepInfo
import io.circe.generic.auto._
import edu.knuca.resmat.http.JsonProtocol._

object ProblemVariantSchemaType extends PimpedEnumeration {
  type ProblemVariantSchemaType = Value
  val ImgUrl = Value(1, "img-url")
  val Geogebra = Value(2, "geogebra")
}

object TaskFlowStepType extends PimpedEnumeration {
  type TaskFlowStepType = Value
  val Test = Value(1, "test")
  val InputSet = Value(2, "input-set")
  val Charts = Value(3, "charts")
  val VariableValueSet = Value(4, "var-value-set")
  val EquationSet = Value(5, "equation-set")
  val DynamicTable = Value(6, "dynamic-table")
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
  val CrossSection = Value(2, "cross-section")
}

object TestType extends PimpedEnumeration {
  type TestType = Value
  val Radio = Value(1, "radio")
  val Checkbox = Value(2, "checkbox")
  val SingleInput = Value(3, "single-input")
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
  val Number = Value(3, "number") // for SingleInput only
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
                        hasToBeSubmitted: Boolean = true) {
  def isReadonly(): Boolean = {
    stepType == ExamStepType.Results
  }
}
@JsonCodec sealed trait ExamStepConfDataSet //to make JsonCodec work
case class ExamStepTestSetDataSet(testSetConfId: Long) extends ExamStepConfDataSet
case class ExamStepTaskFlowDataSet(taskFlowConfId: Long, problemConfId: Long) extends ExamStepConfDataSet
case object ExamStepResultsDataSet extends ExamStepConfDataSet
object ExamStepConfDataSet

case class ExamConfWithStepsDto(examConf: ExamConf, stepConfs: Seq[ExamStepConf])

case class ExamConfCreateDto(examConf: ExamConf, stepConfs: Seq[ExamStepConfCreateDto])
case class ExamConfUpdateDto(examConf: ExamConf, stepConfs: Seq[ExamStepConfUpdateDto])
case class ExamStepConfCreateDto(examStepConf: ExamStepConf, stepDataConf: ExamStepDataConf)
/**
  * @param stepDataConf None if not updated
  */
case class ExamStepConfUpdateDto(examStepConf: ExamStepConf, stepDataConf: Option[ExamStepDataConf])

@JsonCodec sealed trait ExamStepDataConf
case class TestSetConfDto(testSetConf: TestSetConf, testGroups: Seq[TestSetConfTestGroup]) extends ExamStepDataConf
case class TaskFlowConfDto(taskFlowConf: TaskFlowConf, taskFlowSteps: Seq[TaskFlowStepConf]) extends ExamStepDataConf
case object ResultsConf extends ExamStepDataConf
object ExamStepDataConf // to make JsonCodex work

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
                              stepType: ExamStepType.ExamStepType,
                              name: String,
                              attemptsAmount: Int,
                              mistakesAmount: Int,
                              durationMillis: Long,
                              info: Option[UserExamStepResultStepInfo])
@JsonCodec sealed trait UserExamStepResultStepInfo
case class TaskFlowStepResultStepInfo(variant: TaskFlowDto, data: Seq[TaskFlowResultInfoStepDataDto]) extends UserExamStepResultStepInfo
case class TestSetStepResultStepInfo(testSetTests: Seq[UserExamStepAttemptTestSetTest]) extends UserExamStepResultStepInfo
object UserExamStepResultStepInfo

//====================TestSet====================

case class TestSetConf(id: Long, name: String, maxTestsAmount: Int)
case class TestSetConfTestGroup(id: Long, testSetConfId: Long, testGroupConfId: Long, proportionPercents: Int, mistakeValue: Option[Double])

case class TestGroupConf(id: Long, name: String, parentGroupId: Option[Long] = None)
case class TestGroupConfWithAmountOfTestsDto(testGroupConf: TestGroupConf, amountOfTests: Int)
case class TestConf(id: Long,
                    groupId: Long,
                    question: String,
                    imageUrl: Option[String],
                    options: Seq[TestOptionConf],
                    testType: TestType.TestType = TestType.Radio,
                    help: Option[String] = None,
                    precision: Option[Double]= None) {
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
                                          mistakeValue: Option[Double],
                                          done: Boolean = false,
                                          mistakes: Int = 0)

//====================Problem====================

case class ProblemConf(id: Long, name: String, problemType: ProblemType.ProblemType, inputVariableConfs: Seq[ProblemInputVariableConf])
case class ProblemInputVariableConf(id: Int, name: String, units: String = "", alias: String, showInExam: Boolean = true)
case class ProblemInputVariableValue(
  variableConfId: Long,
  value: Double,
  strValue: Option[String] = None,
  variableKey: Option[String] = None //For CrossSection "shapeId.fieldName"
)
//todo switch calculatedData to interface to allow to work with different problems
case class PublicProblemVariantConf(id: Long,
                                    problemConfId: Long,
                                    schemaType: ProblemVariantSchemaType.ProblemVariantSchemaType,
                                    schemaUrl: String,
                                    inputVariableValues: Seq[ProblemInputVariableValue])
case class ProblemVariantConf(id: Long,
                              problemConfId: Long,
                              schemaType: ProblemVariantSchemaType.ProblemVariantSchemaType,
                              schemaUrl: String,
                              inputVariableValues: Seq[ProblemInputVariableValue],
                              calculatedData: ProblemAnswer) {
  val withoutCalculatedData = PublicProblemVariantConf(id, problemConfId, schemaType, schemaUrl, inputVariableValues)
}

//====================TaskFlow====================

case class TaskFlowConf(id: Long, problemConfId: Long, name: String)
case class TaskFlowStepConf(id: Long,
                            taskFlowConfId: Long,
                            sequence: Int,
                            name: String,
                            stepType: TaskFlowStepType.TaskFlowStepType,
                            stepData: String /*JSON field*/,
                            precision: Option[Double] = None,
                            isHelpStep: Boolean = false,
                            isResultInfoStep: Boolean = false)

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
@JsonCodec sealed trait SmartValue {
  def setValue(answer: ProblemAnswer): SmartValue = this
}
case class SmartValueInput(id: Int, answerMapping: String) extends SmartValue
case class SmartValueDynamicDouble(answerMapping: String, value: Double = 0.0, digitsAfterComma: Int = 6) extends SmartValue {
  override def setValue(answer: ProblemAnswer): SmartValue =
    this.copy(value = answer.getDouble(answerMapping))
}
case class SmartValueStaticDouble(value: Double, digitsAfterComma: Int = 6) extends SmartValue
case class SmartValueStaticString(value: String) extends SmartValue

object SmartValue //to make JsonCodec work

case class EquationItem(value: SmartValue, prefix: String = "", suffix: String = "")
case class InputSetEquation(id: Int, items: List[EquationItem])
case class InputSetEquationSystem(name: String, equations: List[InputSetEquation]) extends TaskFlowStepData

case class TaskFlowTestConf(test: TestConf, correctOptionIdsMapping: Option[String] = None) extends TaskFlowStepData

case class ChartData(title: String, x: Array[Double], y: Array[Double], bottom: Boolean = false, positive: Boolean = true) extends TaskFlowStepData
case class ChartSet(title: String, charts: Seq[ChartData])

case class DynamicTableRow(name: String, cells: List[SmartValue])
case class DynamicTable(title: String, colNames: List[String], rows: List[DynamicTableRow]) extends TaskFlowStepData
