package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

case class TestOptionDto(id: Long, value: String, valueType: TestOptionValueType.TestOptionValueType) {
  def this(o: TestOptionConf) = this(o.id, o.value, o.valueType)
}
case class TestDto(id: Long, groupId: Long, testType: TestType.TestType, question: String, help: Option[String], options: Seq[TestOptionDto])
case class TestSetDto(conf: TestSetConf,  tests: Seq[TestDto]) extends StepDataDto

case class TestAnswerDto(testId: Long, submittedOptions: Seq[Long])
case class VerifiedTestAnswerDto(testId: Long, isCorrectAnswer: Boolean, mistakesAmount: Int, answer: Map[Long, Boolean])

class TestSetExamService(val db: DatabaseService)
                        (implicit val executionContext: ExecutionContext) extends LazyLogging {

  private val testSetConfs: List[TestSetConf] = List(
    TestSetConf(1, 1, 1)
  )
  private val testSetConfTestGroups: List[TestSetConfTestGroup] = List(
//    TestSetConfTestGroup(1, 1, 1),
    TestSetConfTestGroup(2, 1, 2)
//    TestSetConfTestGroup(3, 1, 3)
  )

  private val testGroupConfs: List[TestGroupConf] = List(
    TestGroupConf(1, "Group1"),
    TestGroupConf(2, "Group2"),
    TestGroupConf(3, "Group3")
  )

  private val testConfs: List[TestConf] = List(
    TestConf(11, 1, "Test11", Seq(
      TestOptionConf(1, "Option1", true),
      TestOptionConf(2, "Option2"),
      TestOptionConf(3, "Option3"),
      TestOptionConf(4, "Option4")
    )),
    TestConf(12, 1, "Test12", Seq(
      TestOptionConf(1, "Option1", true)
    )),
    TestConf(21, 2, "Test21", Seq(
      TestOptionConf(1, "Option1", true),
      TestOptionConf(2, "Option2")
    ), TestType.Checkbox),
    TestConf(22, 2, "Test22", Seq(
      TestOptionConf(1, "Option1", true)
    )),
    TestConf(31, 3, "Test31", Seq(
      TestOptionConf(1, "Option1", true)
    )),
    TestConf(32, 3, "Test32", Seq(
      TestOptionConf(1, "Option1", true)
    )),
    TestConf(999, 3, "Визначте тип ластини", Seq(
      TestOptionConf(1, "Тонкі", true),
      TestOptionConf(2, "Товсті"),
      TestOptionConf(3, "Мембрани")
    )),
    TestConf(1000, 3, "Чи забезпечуться міцність перерізу?", Seq(
      TestOptionConf(1, "Не забезпечується", true),
      TestOptionConf(2, "Забезпечується")
    ))
  )

  //===============================================================
  //                      User specific data
  //===============================================================

  private val userExamStepAttemptTestSets: ListBuffer[UserExamStepAttemptTestSet] = ListBuffer()

  private val userExamStepAttemptTestSetTests: ListBuffer[UserExamStepAttemptTestSetTest] = ListBuffer()

  //===============================================================
  //                      Code
  //===============================================================

  def getTestSetConf(id: Long): Option[TestSetConf] = testSetConfs.find(_.id == id)

  def getTestSetConfGroups(testSetConfId: Long): Seq[TestSetConfTestGroup] =
    testSetConfTestGroups.filter(_.testSetConfId == testSetConfId)

  def getTestConfs(ids: Seq[Long]): Seq[TestConf] = testConfs.filter(t => ids.contains(t.id))

  def getTestConfsByGroup(groupId: Long): Seq[TestConf] = testConfs.filter(_.groupId == groupId)

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

  def verifyTestSetTestAnswer(stepAttemptTestSetId: Long,
                              testAnswer: TestAnswerDto): Option[VerifiedTestAnswerDto] = {
    val testId = testAnswer.testId
    val attemptTestSet = userExamStepAttemptTestSets.find(_.id == stepAttemptTestSetId).getOrElse(
      throw new RuntimeException(s"Failed to find test set with id: $stepAttemptTestSetId")
    )
    val testSetTest = userExamStepAttemptTestSetTests.find(t => t.stepAttemptTestSetId == attemptTestSet.id && t.testConfId == testId).getOrElse(
      throw new RuntimeException(s"Failed to find test with id: $testId for test set id: ${attemptTestSet.id}")
    )
    verifyTestAnswer(testAnswer).map{ verifiedTestAnswer =>
      //Update information about test submission
      val testIndex = userExamStepAttemptTestSetTests.indexWhere(t =>
        t.stepAttemptTestSetId == testSetTest.stepAttemptTestSetId && t.testConfId == testSetTest.testConfId
      )
      if(verifiedTestAnswer.isCorrectAnswer) {
        userExamStepAttemptTestSetTests.update(testIndex, testSetTest.copy(done = true))
      } else {
        userExamStepAttemptTestSetTests.update(
          testIndex,
          testSetTest.copy(mistakes = testSetTest.mistakes + verifiedTestAnswer.mistakesAmount)
        )
      }
      verifiedTestAnswer
    }
  }

  def verifyTestAnswer(testAnswer: TestAnswerDto): Option[VerifiedTestAnswerDto] = {
    testConfs.find(_.id == testAnswer.testId).map{ test =>
      val correctOptions = test.options.filter(_.correct)
      verifyTestAnswer(testAnswer, correctOptions.map(_.id))
    }
  }

  def verifyTestAnswer(testAnswer: TestAnswerDto, correctOptionIds: Seq[Long]): VerifiedTestAnswerDto = {
    //For every correct option, submitted option exists
    var isCorrectAnswer = correctOptionIds.forall(testAnswer.submittedOptions.contains(_))
    var mistakesAmount = 0
    //For every submitted option, correct option exists
    val verifiedOptions = testAnswer.submittedOptions.map { soId: Long =>
      val correct = correctOptionIds.contains(soId)
      if(!correct) {
        isCorrectAnswer = false
        mistakesAmount = mistakesAmount + 1
      }
      (soId, correct)
    }

    VerifiedTestAnswerDto(testAnswer.testId, isCorrectAnswer, mistakesAmount, verifiedOptions.toMap)
  }

  def getNotCompletedTestConfsInTestSet(testSetId: Long): Seq[TestConf] = {
    val testSetTests = userExamStepAttemptTestSetTests.filter(_.stepAttemptTestSetId == testSetId)
    val notCompletedTestConfIds = testSetTests.filter(!_.done).map(_.testConfId)
    getTestConfs(notCompletedTestConfIds)
  }

  private def testToDto(t: TestConf): TestDto =
    TestDto(t.id, t.groupId, t.testType, t.question, t.help, t.options.map(new TestOptionDto(_)))

}

object TestSetQueries {

}