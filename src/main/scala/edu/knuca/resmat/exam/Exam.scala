package edu.knuca.resmat.exam

import edu.knuca.resmat.utils.PimpedEnumeration
import org.joda.time.DateTime

object TaskFlowStepType extends PimpedEnumeration {
  type TaskFlowStepType = Value
  val Test = Value(1, "test")
  val InputSet = Value(2, "input-set")
  val Charts = Value(3, "charts")
}

object ExamStepType extends PimpedEnumeration {
  type ExamStepType = Value
  val TestSet = Value(1, "test-set")
  val TaskFlow = Value(2, "task-flow")
  val Results = Value(3, "results")
}

object TaskType extends PimpedEnumeration {
  type TaskType = Value
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

case class ExamConf(id: Long, name: String, description: String)
case class ExamStepConf(id: Long,
                        examConfId: Long,
                        sequence: Int,
                        name: String,
                        stepType: ExamStepType.ExamStepType,
                        mistakesPerAttemptLimit: Int,
                        attemptsLimit: Int)
case class ExamStepVariantConf(id: Long, examStepConfId: Long, examConfId: Long, dataSetConfId: Long)

case class UserExam(id: Long, userId: Long, examConfId: Long, currentStepConfId: Long, status: ExamStatus.ExamStatus, started: Option[DateTime], finished: Option[DateTime])
case class UserExamStepAttempt(id: Long, userId: Long, userExamId: Long, examStepConfId: Long, mistakesAmount: Int, attemptNumber: Int/*Starts with 1*/, status: ExamStepStatus.ExamStepStatus, stepVariantConfId: Long)

//====================TestSet====================

case class TestSetConf(id: Long, examConfId: Long, examStepConfId: Long)
case class TestSetConfTestGroup(id: Long, testSetConfId: Long, testGroupId: Long)

case class TestGroupConf(id: Long, name: String)
case class TestConf(id: Long, groupId: Long, question: String, options: Seq[TestOptionConf], testType: TestType.TestType = TestType.Radio, help: Option[String] = None)
case class TestOptionConf(id: Int, value: String, correct: Boolean = false, valueType: TestOptionValueType.TestOptionValueType = TestOptionValueType.Text)

case class UserExamStepAttemptTestSet(id: Long, stepAttemptId: Long, userExamId: Long, examStepConfId: Long, testSetConfId: Long)
case class UserExamStepAttemptTestSetTest(stepAttemptTestSetId: Long, testConfId: Long, done: Boolean = false, mistakes: Int = 0)

//====================TaskFlow====================

case class ProblemConf(id: Long, name: String)
case class ProblemVariantConf(id: Long, problemConfId: Long, variantSpecificData: String)
case class CalculatedProblemVariantConf(id: Long, problemVariantConfId: Long, calculatedData: String)

case class TaskFlowConf(id: Long, problemConfId: Long)
case class TaskFlowStepConf(id: Long,
                            taskFlowConfId: Long,
                            name: String,
                            sequence: Int,
                            stepType: TaskFlowStepType.TaskFlowStepType, stepData: String/*JSON field*/)

case class TaskFlowConfProblemVariantConf(id: Long, taskFlowConfId: Long, problemVariantConfId: Long)

case class UserExamStepAttemptTaskFlow(id: Long, stepAttemptId: Long, userExamId: Long, examStepConfId: Long, taskFlowConfId: Long, problemVariantConfId: Long, currentStepId: Long)
case class UserExamStepAttemptTaskFlowStep(id: Long, stepAttemptTaskFlowId: Long, taskFlowStepConfId: Long, done: Boolean = false, mistakes: Int = 0)

sealed trait TaskFlowStepData

case class InputSet(id: Long, name: String, inputs: Seq[InputSetInput], answer: InputSetAnswerDto) extends TaskFlowStepData
case class InputSetInput(id: Int, //unique within input set
                         name: String,
                         groupName: String,
                         units: String,
                         correctValueVariableName: String = "", //to lookup correct value in CalculatedProblemVariantConf
                         description: String = "")

case class TaskFlowTest(testId: Long) extends TaskFlowStepData

