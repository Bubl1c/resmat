package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

case class TestDto(id: Long, groupId: Long, testType: TestType.TestType, question: String, help: Option[String], options: Seq[TestOption])
case class TestSetDto(conf: TestSetConf,  tests: Seq[TestDto]) extends StepData

class TestSetExamService(val db: DatabaseService)
                        (implicit val executionContext: ExecutionContext) extends LazyLogging {

  private val testSetConfs: List[TestSetConf] = List(
    TestSetConf(1, 1, 1, 6, 3)
  )
  private val testSetConfTestGroups: List[TestSetConfTestGroup] = List(
    TestSetConfTestGroup(1, 1, 1),
    TestSetConfTestGroup(2, 1, 2),
    TestSetConfTestGroup(3, 1, 3)
  )

  private val testGroups: List[TestGroup] = List(
    TestGroup(1, "Group1"),
    TestGroup(2, "Group2"),
    TestGroup(3, "Group3")
  )

  private val tests: List[Test] = List(
    Test(11, 1, "Test11"),
    Test(12, 1, "Test12"),
    Test(21, 2, "Test21"),
    Test(22, 2, "Test22"),
    Test(31, 3, "Test31"),
    Test(32, 3, "Test32")
  )
  private val testOptions: List[TestOption] = List(
    TestOption(111, 11, 1, "Option1", true),
    TestOption(121, 12, 1, "Option1", true),
    TestOption(211, 21, 1, "Option1", true),
    TestOption(221, 22, 1, "Option1", true),
    TestOption(311, 31, 1, "Option1", true),
    TestOption(321, 32, 1, "Option1", true)
  )

  //===============================================================
  //                      User specific data
  //===============================================================

  private val userExamStepAttemptTestSets: ListBuffer[UserExamStepAttemptTestSet] = ListBuffer(
    UserExamStepAttemptTestSet(1, 1, 1, 1)
  )
  private val userExamStepAttemptTests: ListBuffer[UserExamStepAttemptTest] = ListBuffer(
    UserExamStepAttemptTest(1, 11),
    UserExamStepAttemptTest(1, 21),
    UserExamStepAttemptTest(1, 31)
  )

  //===============================================================
  //                      Code
  //===============================================================

  def getTestSetConf(id: Long): Option[TestSetConf] = testSetConfs.find(_.id == id)

  def getTestSetConfGroups(testSetConfId: Long): Seq[TestSetConfTestGroup] =
    testSetConfTestGroups.filter(_.testSetConfId == testSetConfId)

  def getTests(ids: Seq[Long]): Seq[Test] = tests.filter(t => ids.contains(t.id))

  def getTestsByGroup(groupId: Long): Seq[Test] = tests.filter(_.groupId == groupId)

  def getTestOptions(testId: Long): Seq[TestOption] = testOptions.filter(_.testId == testId)

  def getTestSet(id: Long): Option[UserExamStepAttemptTestSet] =
    userExamStepAttemptTestSets.find(_.id == id)

  def getTestSetDto(testSetId: Long): Option[TestSetDto] = {
    getTestSet(testSetId).flatMap{ testSet =>
      val testSetConfOpt = getTestSetConf(testSet.testSetConfId)
      testSetConfOpt.map{ testSetConf =>
        val testSetTests = getTestsByTestSet(testSet.id)
        val testDtos = testSetTests.map(testToDto)
        TestSetDto(testSetConf, testDtos)
      }
    }
  }

  def getTestsByTestSet(testSetId: Long): Seq[Test] = {
    val testSetTests = userExamStepAttemptTests.filter(_.stepAttemptTestSetId == testSetId)
    if(testSetTests.isEmpty) {
      Seq()
    } else {
      val testIds = testSetTests.map(_.testId)
      tests.filter(t => testIds.contains(t.id))
    }
  }

  def takeTestsFromGroups(groupIds: Seq[Long], testsAmount: Int): Seq[Test] =
    groupIds.flatMap(getTestsByGroup(_).take(testsAmount))  //todo reorder tests

  def testToDto(t: Test): TestDto = TestDto(t.id, t.groupId, t.testType, t.question, t.help, getTestOptions(t.id))

  def createTestSetWithTests(testSet: UserExamStepAttemptTestSet): (UserExamStepAttemptTestSet, Seq[Test]) = {
    val newTestSet = createTestSet(testSet)

    val tscTestGroups = getTestSetConfGroups(newTestSet.testSetConfId)
    val testSetTestsFromGroups: Seq[Test] = takeTestsFromGroups(tscTestGroups.map(_.id), 1)

    createTestSetTests(testSetTestsFromGroups.map(t => UserExamStepAttemptTest(newTestSet.id, t.id)))

    (newTestSet, testSetTestsFromGroups)
  }

  def createTestSet(testSet: UserExamStepAttemptTestSet): UserExamStepAttemptTestSet = {
    val userExamStepAttemptTestSetNextId = if(userExamStepAttemptTestSets.nonEmpty) userExamStepAttemptTestSets.last.id + 1 else 1
    val withId = testSet.copy(id = userExamStepAttemptTestSetNextId)
    userExamStepAttemptTestSets += withId
    withId
  }

  def createTestSetTest(test: UserExamStepAttemptTest): UserExamStepAttemptTest = {
    userExamStepAttemptTests += test
    test
  }

  def createTestSetTests(tests: Seq[UserExamStepAttemptTest]): Seq[UserExamStepAttemptTest] = {
    userExamStepAttemptTests ++= tests
    tests
  }

}

object TestSetQueries {

}