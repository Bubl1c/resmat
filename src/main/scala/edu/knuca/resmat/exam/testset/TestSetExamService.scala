package edu.knuca.resmat.exam.testset

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam._
import edu.knuca.resmat.tests.TestConfService
import edu.knuca.resmat.utils.SqlUtils

import scala.concurrent.ExecutionContext

case class TestOptionDto(id: Long, value: String, valueType: TestOptionValueType.TestOptionValueType) {
  def this(o: TestOptionConf) = this(o.id, o.value, o.valueType)
}
case class TestDto(id: Long, groupId: Long, testType: TestType.TestType, question: String, imageUrl: Option[String], help: Option[String], options: Seq[TestOptionDto])
case class TestSetDto(conf: TestSetConf,  tests: Seq[TestDto]) extends StepDataDto

sealed trait TestSubmittedAnswer
case class TestSubmittedAnswerDto(testConfId: Long, submittedOptions: Seq[Long]) extends TestSubmittedAnswer
case class TestSingleInputSubmittedAnswer(submittedAnswer: String) extends TestSubmittedAnswer
case class VerifiedTestAnswerDto(testId: Long, isCorrectAnswer: Boolean, mistakesAmount: Int, answer: Map[Long, Boolean])

class TestSetExamService(val db: DatabaseService, val testConfsService: TestConfService)
                        (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.exam.testset.{TestSetQueries => Q}

  //====================UserExamStepAttemptTestSet====================

  def getTestConfsByTestSet(testSetId: Long): Seq[TestConf] = {
    val testSetTests = findStepAttemptTestsByStepAttemptTestSetId(testSetId)
    if(testSetTests.isEmpty) {
      Seq()
    } else {
      val testConfIds = testSetTests.map(_.testConfId)
      testConfsService.findTestConfs(testConfIds)
    }
  }

  def getNotCompletedTestConfsInTestSet(testSetId: Long): Seq[TestConf] = {
    val testSetTests = findStepAttemptTestsByStepAttemptTestSetId(testSetId)
    val notCompletedTestConfIds = testSetTests.filter(!_.done).map(_.testConfId)
    if(notCompletedTestConfIds.nonEmpty) {
      testConfsService.findTestConfs(notCompletedTestConfIds)
    } else {
      Seq()
    }
  }

  def createUserExamTestSet(testSet: UserExamStepAttemptTestSet): UserExamStepAttemptTestSet = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createUserExamTestSet(testSet).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $testSet")
    )
    getUserExamTestSet(insertedId)
  }

  def getUserExamTestSet(id: Long): UserExamStepAttemptTestSet = db.run { implicit c =>
    Q.getUserExamTestSet(id).as(Q.uetsParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Failed to find step attempt test set with id: $id")
    )
  }

  def getUserExamTestSetByAttemptId(stepAttemptId: Long): UserExamStepAttemptTestSet = db.run { implicit c =>
    Q.getUserExamTestSetByAttemptId(stepAttemptId).as(Q.uetsParser.singleOpt).getOrElse(
      throw new RuntimeException(s"UserExamStepAttemptTestSet with stepAttemptId $stepAttemptId not found")
    )
  }

  //====================UserExamStepAttemptTestSetTest====================

  def createUserExamTestSetTest(test: UserExamStepAttemptTestSetTest): UserExamStepAttemptTestSetTest = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createUserExamTestSetTest(test).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $test")
    )
    getTestSetTest(insertedId)
  }

  def findStepAttemptTestsByStepAttemptTestSetId(stepAttemptTestSetId: Long): Seq[UserExamStepAttemptTestSetTest] = db.run { implicit c =>
    Q.findStepAttemptTestsByStepAttemptTestSetId(stepAttemptTestSetId).as(Q.uetstParser.*)
  }

  def getTestSetTest(id: Long): UserExamStepAttemptTestSetTest = db.run { implicit c =>
    Q.getUserExamTestSetTest(id).as(Q.uetstParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Failed to find UserExamStepAttemptTestSetTest with id: $id")
    )
  }

  def getTestSetTest(testSetId: Long, testConfId: Long): UserExamStepAttemptTestSetTest = db.run { implicit c =>
    Q.getStepAttemptTest(testSetId, testConfId).as(Q.uetstParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Failed to find test with id: $testConfId for test set id: $testSetId")
    )
  }

  def findTestSetTests(stepAttemptIds: Seq[Long]): Seq[UserExamStepAttemptTestSetTest] = db.run { implicit c =>
    if (stepAttemptIds.isEmpty) {
      Seq.empty
    } else {
      Q.findUserExamTestSetTestsByStepAttemptIds(stepAttemptIds).as(Q.uetstParser.*)
    }
  }

  def updateTestSetTest(test: UserExamStepAttemptTestSetTest): UserExamStepAttemptTestSetTest = db.run { implicit c =>
    val affectedRows = Q.updateUserExamTestSetTest(test).executeUpdate()
    if(affectedRows != 1) {
      throw new RuntimeException(s"Failed to update $test")
    }
    getTestSetTest(test.id)
  }

  def getTestSetDto(stepAttemptId: Long): TestSetDto = {
    val testSet = getUserExamTestSetByAttemptId(stepAttemptId)
    val testSetConf = testConfsService.getTestSetConf(testSet.testSetConfId)
    val testSetTestConfs = getTestConfsByTestSet(testSet.id)
    val testDtos = testSetTestConfs.map(testToDto)
    TestSetDto(testSetConf, testDtos)
  }

  def createTestSetWithTests(testSet: UserExamStepAttemptTestSet, testsAmount: Int): (UserExamStepAttemptTestSet, Seq[TestConf]) = {
    val newTestSet = createUserExamTestSet(testSet)

    val tscTestGroups = testConfsService.findTestSetConfGroups(newTestSet.testSetConfId)
    val testSetTestsFromGroups: Seq[(TestSetConfTestGroup, Seq[TestConf])] = testConfsService.takeTestConfsFromGroups(
      tscTestGroups.map { tg =>
        val proportion = tg.proportionPercents / 100.0
        (tg, Math.ceil(testsAmount * proportion).toInt)
      }
    )

    testSetTestsFromGroups
      .flatMap{ case (testGroup, testConfs) =>
        testConfs.map(testConf => UserExamStepAttemptTestSetTest(-1, newTestSet.id, testConf.id, Some(testConf), testGroup.mistakeValue))
      }
      .foreach(createUserExamTestSetTest)

    (newTestSet, testSetTestsFromGroups.flatMap(_._2))
  }

  def verifyTestSetTestAnswer(stepAttemptTestSetId: Long,
                              testConfId: Long,
                              answer: TestSubmittedAnswer): VerifiedTestAnswerDto = {
    val testId = testConfId
    val testSetTest = getTestSetTest(stepAttemptTestSetId, testId)
    val testConf = testConfsService.getTestConf(testConfId)
    val correctOptions = testConf.options.filter(_.correct)
    val verifiedTestAnswer = verifyTestAnswer(testConf, answer, correctOptions)
    //Update information about test submission
    if(verifiedTestAnswer.isCorrectAnswer) {
      updateTestSetTest(testSetTest.copy(done = true))
    } else {
      updateTestSetTest(testSetTest.copy(mistakes = testSetTest.mistakes + verifiedTestAnswer.mistakesAmount))
    }
    verifiedTestAnswer
  }

  private def verifyTestAnswer(testConf: TestConf,
                               answer: TestSubmittedAnswer,
                               correctOptions: Seq[TestOptionConf]): VerifiedTestAnswerDto = {
    testConf.testType match {
      case TestType.SingleInput =>
        val submitted = answer.asInstanceOf[TestSingleInputSubmittedAnswer]
        val correct = correctOptions.headOption.getOrElse(
          throw new IllegalStateException(s"No correct options for test conf with id ${testConf.id}")
        )
        TestUtils.verifySingleInputTest(testConf.id, submitted.submittedAnswer, correct, testConf.precision)
      case TestType.Radio | TestType.Checkbox =>
        val submitted = answer.asInstanceOf[TestSubmittedAnswerDto]
        TestUtils.verifyTraditionalTest(submitted, correctOptions.map(_.id))
      case _ =>
        throw new IllegalStateException(s"Unsupported TestType ${testConf.testType} while verifying test answer")
    }
  }

  private def testToDto(t: TestConf): TestDto =
    TestDto(t.id, t.groupId, t.testType, t.question, t.imageUrl, t.help, t.options.map(new TestOptionDto(_)))

}

object TestSetQueries {
  import anorm.SqlParser.{bool, double, int, long, str}

  object UETS {
    val table = "user_exam_step_attempt_test_sets"
    val id = "id"
    val stepAttemptId = "step_attempt_id"
    val testSetConfId = "test_set_conf_id"
  }

  object UETST {
    val table = "user_exam_step_attempt_test_set_tests"
    val id = "id"
    val stepAttemptTestSetId = "step_attempt_test_set_id"
    val testConfId = "test_conf_id"
    val testConfSnapshot = "test_conf_snapshot"
    val mistakeValue = "mistake_value"
    val done = "done"
    val mistakes = "mistakes"
  }

  val uetsParser  = for {
    id <- long(UETS.id)
    stepAttemptId <- long(UETS.stepAttemptId)
    testSetConfId <- long(UETS.testSetConfId)
  } yield UserExamStepAttemptTestSet(id, stepAttemptId, testSetConfId)

  val uetstParser  = for {
    id <- long(UETST.id)
    stepAttemptTestSetId <- long(UETST.stepAttemptTestSetId)
    testConfId <- long(UETST.testConfId)
    testConfSnapshot <- str(UETST.testConfSnapshot).?
    mistakeValue <- double(UETST.mistakeValue).?
    done <- bool(UETST.done)
    mistakes <- int(UETST.mistakes)
  } yield UserExamStepAttemptTestSetTest(id, stepAttemptTestSetId, testConfId, testConfSnapshot.map(JsonTools.decodeTC), mistakeValue, done, mistakes)

  def createUserExamTestSet(uets: UserExamStepAttemptTestSet) =
    SQL(
      s"""INSERT INTO ${UETS.table} (
         |${UETS.stepAttemptId},
         |${UETS.testSetConfId}
         |) VALUES (
         |{stepAttemptId},
         |{testSetConfId}
         |)""".stripMargin)
      .on("stepAttemptId" -> uets.stepAttemptId)
      .on("testSetConfId" -> uets.testSetConfId)

  def createUserExamTestSetTest(uetst: UserExamStepAttemptTestSetTest) =
    SQL(
      s"""INSERT INTO ${UETST.table} (
         |${UETST.stepAttemptTestSetId},
         |${UETST.testConfId},
         |${UETST.testConfSnapshot},
         |${UETST.mistakeValue},
         |${UETST.done},
         |${UETST.mistakes}
         |) VALUES (
         |{stepAttemptTestSetId},
         |{testConfId},
         |{testConfSnapshot},
         |{mistakeValue},
         |{done},
         |{mistakes}
         |)""".stripMargin)
      .on("stepAttemptTestSetId" -> uetst.stepAttemptTestSetId)
      .on("testConfId" -> uetst.testConfId)
      .on("testConfSnapshot" -> uetst.testConfSnapshot.map(JsonTools.encodeTC).orNull)
      .on("mistakeValue" -> uetst.mistakeValue)
      .on("done" -> uetst.done)
      .on("mistakes" -> uetst.mistakes)

  def updateUserExamTestSetTest(uetst: UserExamStepAttemptTestSetTest) =
    SQL(
      s"""UPDATE ${UETST.table} SET
         |${UETST.done}={done},
         |${UETST.mistakes}={mistakes}
         |WHERE ${UETST.id} = {id}
         |""".stripMargin)
      .on("id" -> uetst.id)
      .on("done" -> uetst.done)
      .on("mistakes" -> uetst.mistakes)

  def findStepAttemptTestsByStepAttemptTestSetId(stepAttemptTestSetId: Long) =
    SQL(s"SELECT * FROM ${UETST.table} WHERE ${UETST.stepAttemptTestSetId} = {stepAttemptTestSetId}")
      .on("stepAttemptTestSetId" -> stepAttemptTestSetId)

  def getStepAttemptTest(stepAttemptTestSetId: Long, testConfId: Long) =
    SQL(s"SELECT * FROM ${UETST.table} WHERE ${UETST.stepAttemptTestSetId} = {stepAttemptTestSetId} AND ${UETST.testConfId} = {testConfId}")
      .on("stepAttemptTestSetId" -> stepAttemptTestSetId)
      .on("testConfId" -> testConfId)

  def getUserExamTestSet(id: Long) = SqlUtils.get(UETS.table, id)

  def getUserExamTestSetTest(id: Long) = SqlUtils.get(UETST.table, id)

  def findUserExamTestSetTestsByStepAttemptIds(stepAttemptIds: Seq[Long]) =
    SQL(
      s"""
         |SELECT * FROM ${UETST.table} uetst
         |JOIN ${UETS.table} uets ON uets.id = uetst.${UETST.stepAttemptTestSetId}
         |WHERE ${UETS.stepAttemptId} IN ({stepAttemptIds})
       """.stripMargin
    ).on("stepAttemptIds" -> stepAttemptIds)

  def getUserExamTestSetByAttemptId(stepAttemptId: Long) =
    SQL(s"SELECT * FROM ${UETS.table} WHERE ${UETS.stepAttemptId} = {stepAttemptId}").on("stepAttemptId" -> stepAttemptId)
}

object JsonTools {
  import io.circe.parser._
  import io.circe.syntax._
  import io.circe.generic.auto._
  import edu.knuca.resmat.http.JsonProtocol._
  
  def encodeTC(tc: TestConf): String = {
    tc.asJson.toString()
  }

  def decodeTC(json: String): TestConf = decode[TestConf](json).fold( e =>
    throw new RuntimeException(s"Failed to decode TestConf in json: $json", e),
    r => r
  )
  
}