package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

case class TestOptionDto(id: Long, testId: Long, sequence: Int, value: String, valueType: TestOptionValueType.TestOptionValueType) {
  def this(o: TestOptionConf) = this(o.id, o.testConfId, o.sequence, o.value, o.valueType)
}
case class TestDto(id: Long, groupId: Long, testType: TestType.TestType, question: String, help: Option[String], options: Seq[TestOptionDto])
case class TestSetDto(conf: TestSetConf,  tests: Seq[TestDto]) extends StepDataDto

case class VerifiedTestAnswerDto(testId: Long, isCorrectAnswer: Boolean, mistakesAmount: Int, answer: Map[Long, Boolean])

class TestSetExamService(val db: DatabaseService)
                        (implicit val executionContext: ExecutionContext) extends LazyLogging {

  private val testSetConfs: List[TestSetConf] = List(
    TestSetConf(1, 1, 1)
  )
  private val testSetConfTestGroups: List[TestSetConfTestGroup] = List(
    TestSetConfTestGroup(1, 1, 1),
    TestSetConfTestGroup(2, 1, 2),
    TestSetConfTestGroup(3, 1, 3)
  )

  private val testGroupConfs: List[TestGroupConf] = List(
    TestGroupConf(1, "Group1"),
    TestGroupConf(2, "Group2"),
    TestGroupConf(3, "Group3")
  )

  private val testConfs: List[TestConf] = List(
    TestConf(11, 1, "Test11"),
    TestConf(12, 1, "Test12"),
    TestConf(21, 2, "Test21"),
    TestConf(22, 2, "Test22"),
    TestConf(31, 3, "Test31"),
    TestConf(32, 3, "Test32")
  )
  private val testOptionConfs: List[TestOptionConf] = List(
    TestOptionConf(111, 11, 1, "Option1", true),
    TestOptionConf(112, 11, 2, "Option2", false),
    TestOptionConf(113, 11, 3, "Option3", false),
    TestOptionConf(114, 11, 4, "Option4", false),
    TestOptionConf(121, 12, 1, "Option1", true),
    TestOptionConf(211, 21, 1, "Option1", true),
    TestOptionConf(221, 22, 1, "Option1", true),
    TestOptionConf(311, 31, 1, "Option1", true),
    TestOptionConf(321, 32, 1, "Option1", true)
  )

  //===============================================================
  //                      User specific data
  //===============================================================

  private val userExamStepAttemptTestSets: ListBuffer[UserExamStepAttemptTestSet] = ListBuffer(
    UserExamStepAttemptTestSet(1, 1, 1, 1, 1)
  )
  private val userExamStepAttemptTestSetTests: ListBuffer[UserExamStepAttemptTestSetTest] = ListBuffer(
    UserExamStepAttemptTestSetTest(1, 11),
    UserExamStepAttemptTestSetTest(1, 21),
    UserExamStepAttemptTestSetTest(1, 31)
  )

  //===============================================================
  //                      Code
  //===============================================================

  def getTestSetConf(id: Long): Option[TestSetConf] = testSetConfs.find(_.id == id)

  def getTestSetConfGroups(testSetConfId: Long): Seq[TestSetConfTestGroup] =
    testSetConfTestGroups.filter(_.testSetConfId == testSetConfId)

  def getTestConfs(ids: Seq[Long]): Seq[TestConf] = testConfs.filter(t => ids.contains(t.id))

  def getTestConfsByGroup(groupId: Long): Seq[TestConf] = testConfs.filter(_.groupId == groupId)

  def getTestOptionConfs(testId: Long): Seq[TestOptionConf] = testOptionConfs.filter(_.testConfId == testId)

  def getTestConfsByTestSet(testSetId: Long): Seq[TestConf] = {
    val testSetTests = userExamStepAttemptTestSetTests.filter(_.stepAttemptTestSetId == testSetId)
    if(testSetTests.isEmpty) {
      Seq()
    } else {
      val testIds = testSetTests.map(_.testConfId)
      testConfs.filter(t => testIds.contains(t.id))
    }
  }

  def takeTestConfsFromGroups(groupIds: Seq[Long], testsAmount: Int): Seq[TestConf] =
    groupIds.flatMap(getTestConfsByGroup(_).take(testsAmount))  //todo reorder tests

  def getTestSet(id: Long): Option[UserExamStepAttemptTestSet] =
    userExamStepAttemptTestSets.find(_.id == id)

  def getTestSetByAttemptId(stepAttemptId: Long): Option[UserExamStepAttemptTestSet] =
    userExamStepAttemptTestSets.find(_.stepAttemptId == stepAttemptId)

  def getTestSetDto(stepAttemptId: Long): Option[TestSetDto] = {
    getTestSetByAttemptId(stepAttemptId).flatMap{ testSet =>
      val testSetConfOpt = getTestSetConf(testSet.testSetConfId)
      testSetConfOpt.map{ testSetConf =>
        val testSetTests = getTestConfsByTestSet(testSet.id)
        val testDtos = testSetTests.map(testToDto)
        TestSetDto(testSetConf, testDtos)
      }
    }
  }

  def createTestSetWithTests(testSet: UserExamStepAttemptTestSet): (UserExamStepAttemptTestSet, Seq[TestConf]) = {
    val newTestSet = createTestSet(testSet)

    val tscTestGroups = getTestSetConfGroups(newTestSet.testSetConfId)
    val testSetTestsFromGroups: Seq[TestConf] = takeTestConfsFromGroups(tscTestGroups.map(_.id), 1)

    testSetTestsFromGroups
      .map(t => UserExamStepAttemptTestSetTest(newTestSet.id, t.id))
      .foreach(createTestSetTest)

    (newTestSet, testSetTestsFromGroups)
  }

  def createTestSet(testSet: UserExamStepAttemptTestSet): UserExamStepAttemptTestSet = {
    val userExamStepAttemptTestSetNextId = if(userExamStepAttemptTestSets.nonEmpty) userExamStepAttemptTestSets.last.id + 1 else 1
    val withId = testSet.copy(id = userExamStepAttemptTestSetNextId)
    userExamStepAttemptTestSets += withId
    withId
  }

  def createTestSetTest(test: UserExamStepAttemptTestSetTest): UserExamStepAttemptTestSetTest = {
    userExamStepAttemptTestSetTests += test
    test
  }

  def verifyTestAnswer(stepAttemptTestSetId: Long,
                       testId: Long,
                       submittedOptions: Seq[Long]): Option[VerifiedTestAnswerDto] = {
    val attemptTestSet = userExamStepAttemptTestSets.find(_.id == stepAttemptTestSetId).getOrElse(
      throw new RuntimeException(s"Failed to find test set with id: $stepAttemptTestSetId")
    )
    val testSetTest = userExamStepAttemptTestSetTests.find(t => t.stepAttemptTestSetId == attemptTestSet.id && t.testConfId == testId).getOrElse(
      throw new RuntimeException(s"Failed to find test with id: $testId for test set id: ${attemptTestSet.id}")
    )
    testConfs.find(_.id == testId).map{ test =>
      val correctOptions = getTestOptionConfs(test.id).filter(_.correct)
      //For every correct option, submitted option exists
      var isCorrectAnswer = correctOptions.forall(co => submittedOptions.contains(co.id))
      var mistakesAmount = 0
      //For every submitted option, correct option exists
      val verifiedOptions = submittedOptions.map { so: Long =>
        val correct = correctOptions.exists(_.id == so)
        if(!correct) {
          isCorrectAnswer = false
          mistakesAmount = mistakesAmount + 1
        }
        (so, correct)
      }

      //Update information about test submission
      val testIndex = userExamStepAttemptTestSetTests.indexWhere(t => t.stepAttemptTestSetId == testSetTest.stepAttemptTestSetId && t.testConfId == testSetTest.testConfId)
      if(isCorrectAnswer) {
        userExamStepAttemptTestSetTests.update(testIndex, testSetTest.copy(done = true))
      } else {
        userExamStepAttemptTestSetTests.update(testIndex, testSetTest.copy(mistakes = testSetTest.mistakes + mistakesAmount))
      }

      VerifiedTestAnswerDto(testId, isCorrectAnswer, mistakesAmount, verifiedOptions.toMap)
    }
  }

  def getNotCompletedTestConfsInTestSet(testSetId: Long): Seq[TestConf] = {
    val testSetTests = userExamStepAttemptTestSetTests.filter(_.stepAttemptTestSetId == testSetId)
    val notCompletedTestConfIds = testSetTests.filter(!_.done).map(_.testConfId)
    getTestConfs(notCompletedTestConfIds)
  }

  private def testToDto(t: TestConf): TestDto =
    TestDto(t.id, t.groupId, t.testType, t.question, t.help, getTestOptionConfs(t.id).map(new TestOptionDto(_)))

}

object TestSetQueries {

}