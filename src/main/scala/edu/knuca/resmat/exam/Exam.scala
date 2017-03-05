package edu.knuca.resmat.exam

import edu.knuca.resmat.utils.PimpedEnumeration
import org.joda.time.DateTime

object ExamStepDataType extends PimpedEnumeration {
  type ExamStepDataType = Value
  val TestSet = Value(1, "test-set")
  val TaskFlow = Value(2, "task-flow")
  val Results = Value(3, "results")
  val Test = Value(4, "test")
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

case class UserExam(id: Long, userId: Long, examConfId: Long, variant: Int, currentStepId: Long, status: ExamStatus.ExamStatus, started: Option[DateTime], finished: Option[DateTime])
case class UserExamStepAttempt(id: Long, userId: Long, userExamId: Long, examStepConfId: Long, mistakesAmount: Int, attemptNumber: Int/*Starts with 1*/, status: ExamStepStatus.ExamStepStatus, stepVariantConfId: Long, dataSetId: Long)
case class UserExamStepAttemptTestSet(id: Long, userExamId: Long, examStepConfId: Long, testSetConfId: Long)
case class UserExamStepAttemptTest(stepAttemptTestSetId: Long, testId: Long)



case class ExamConf(id: Long, name: String, description: String)

case class ExamStepConf(id: Long,
                        examConfId: Long,
                        sequence: Int,
                        name: String,
                        stepType: ExamStepDataType.ExamStepDataType)
case class ExamStepVariantConf(id: Long, examStepConfId: Long, examId: Long, dataSetConfId: Long)



//case class TaskFlowConf(id: Long, taskConfId: Long)
//case class TaskFlowStepConf(id: Long,
//                            taskFlowConfId: Long,
//                            name: String,
//                            sequence: Int,
//                            stepType: ExamStepDataType.ExamStepDataType,
//                            dataConfigurationId: Long)
//
//case class TaskConf(id: Long, name: String, taskType: TaskType.TaskType, inputData: String) //JSON
//case class TaskVariant(id: Long, taskConfId: Long, inputData: String) //JSON
//case class UserTaskVariant(id: Long, userId: Long, taskVariantId: Long)


case class TestSetConf(id: Long, examConfId: Long, examStepConfId: Long, mistakesPerAttemptLimit: Int, attemptsLimit: Int)
case class TestSetConfTestGroup(id: Long, testSetConfId: Long, testGroupId: Long)

case class TestGroup(id: Long, name: String)
case class Test(id: Long, groupId: Long, question: String, testType: TestType.TestType = TestType.Radio, help: Option[String] = None)
case class TestOption(id: Long, testId: Long, sequence: Int, value: String, correct: Boolean = false, valueType: TestOptionValueType.TestOptionValueType = TestOptionValueType.Text)

case class InputSet(id: Long, name: String)
case class InputSetInput(id: Long,
                         inputSetId: Long,
                         name: String,
                         groupName: String,
                         units: String,
                         correctValue: Option[String],
                         description: String)