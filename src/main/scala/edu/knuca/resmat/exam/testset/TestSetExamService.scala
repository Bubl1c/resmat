package edu.knuca.resmat.exam.testset

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

case class TestOptionDto(id: Long, value: String, valueType: TestOptionValueType.TestOptionValueType) {
  def this(o: TestOptionConf) = this(o.id, o.value, o.valueType)
}
case class TestDto(id: Long, groupId: Long, testType: TestType.TestType, question: String, imageUrl: Option[String], help: Option[String], options: Seq[TestOptionDto])
case class TestSetDto(conf: TestSetConf,  tests: Seq[TestDto]) extends StepDataDto

case class TestAnswerDto(testId: Long, submittedOptions: Seq[Long])
case class VerifiedTestAnswerDto(testId: Long, isCorrectAnswer: Boolean, mistakesAmount: Int, answer: Map[Long, Boolean])

class TestSetExamService(val db: DatabaseService)
                        (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.exam.testset.{TestSetQueries => Q}

  //====================TestSetConf====================

  def createTestSetConf(testSetConf: TestSetConf): TestSetConf = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTestSetConf(testSetConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $testSetConf")
    )
    getTestSetConf(insertedId)
  }

  def getTestSetConf(id: Long): TestSetConf = db.run { implicit c =>
    Q.getTestSetConf(id).as(Q.tscParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Test set conf with id $id not found")
    )
  }

  //====================TestGroupConf====================

  def createTestGroupConf(testGroupConf: TestGroupConf): TestGroupConf = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTestGroupConf(testGroupConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $testGroupConf")
    )
    getTestGroupConf(insertedId)
  }

  def getTestGroupConf(id: Long): TestGroupConf = db.run { implicit c =>
    Q.getTestGroupConf(id).as(Q.tgcParser.singleOpt).getOrElse(
      throw new RuntimeException(s"TestGroupConf with id $id not found")
    )
  }

  //====================TestSetConfTestGroup====================

  def createTestSetConfTestGroup(tsctg: TestSetConfTestGroup): TestSetConfTestGroup = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTestSetConfTestGroup(tsctg).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $tsctg")
    )
    getTestSetConfTestGroup(insertedId)
  }

  def getTestSetConfTestGroup(id: Long): TestSetConfTestGroup = db.run { implicit c =>
    Q.getTestSetConfTestGroup(id).as(Q.tsctgcParser.singleOpt).getOrElse(
      throw new RuntimeException(s"TestSetConfTestGroup with id $id not found")
    )
  }

  def findTestSetConfGroups(testSetConfId: Long): Seq[TestSetConfTestGroup] = db.run { implicit c =>
    Q.findTestSetConfGroups(testSetConfId).as(Q.tsctgcParser.*)
  }

  //====================TestConf====================

  def createTestConf(testConf: TestConf): TestConf = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTestConf(testConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $testConf")
    )
    getTestConf(insertedId)
  }

  def getTestConf(id: Long): TestConf = db.run { implicit c =>
    Q.getTestConf(id).as(Q.tcParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Test conf with id $id not found")
    )
  }

  def findTestConfs(ids: Seq[Long]): Seq[TestConf] = db.run { implicit c =>
    Q.findTestConfs(ids).as(Q.tcParser.*)
  }

  def getNotCompletedTestConfsInTestSet(testSetId: Long): Seq[TestConf] = {
    val testSetTests = findStepAttemptTestsByStepAttemptTestSetId(testSetId)
    val notCompletedTestConfIds = testSetTests.filter(!_.done).map(_.testConfId)
    if(notCompletedTestConfIds.nonEmpty) {
      findTestConfs(notCompletedTestConfIds)
    } else {
      Seq()
    }
  }

  def findTestConfsByGroup(groupId: Long): Seq[TestConf] = db.run { implicit c =>
    Q.findTestConfsByGroup(groupId).as(Q.tcParser.*)
  }

  def getTestConfsByTestSet(testSetId: Long): Seq[TestConf] = {
    val testSetTests = findStepAttemptTestsByStepAttemptTestSetId(testSetId)
    if(testSetTests.isEmpty) {
      Seq()
    } else {
      val testConfIds = testSetTests.map(_.testConfId)
      findTestConfs(testConfIds)
    }
  }

  def takeTestConfsFromGroups(groupIdsWithProportions: Seq[(Long, Int)]): Seq[TestConf] =
    groupIdsWithProportions.flatMap{ case(groupId, proportion) =>
      findTestConfsByGroup(groupId).take(proportion)
    }  //todo reorder tests

  //====================UserExamStepAttemptTestSet====================

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

  def updateTestSetTest(test: UserExamStepAttemptTestSetTest): UserExamStepAttemptTestSetTest = db.run { implicit c =>
    val affectedRows = Q.updateUserExamTestSetTest(test).executeUpdate()
    if(affectedRows != 1) {
      throw new RuntimeException(s"Failed to update $test")
    }
    getTestSetTest(test.id)
  }

  def getTestSetDto(stepAttemptId: Long): TestSetDto = {
    val testSet = getUserExamTestSetByAttemptId(stepAttemptId)
    val testSetConf = getTestSetConf(testSet.testSetConfId)
    val testSetTests = getTestConfsByTestSet(testSet.id)
    val testDtos = testSetTests.map(testToDto)
    TestSetDto(testSetConf, testDtos)
  }

  def createTestSetWithTests(testSet: UserExamStepAttemptTestSet, testsAmount: Int): (UserExamStepAttemptTestSet, Seq[TestConf]) = {
    val newTestSet = createUserExamTestSet(testSet)

    val tscTestGroups = findTestSetConfGroups(newTestSet.testSetConfId)
    val testSetTestsFromGroups: Seq[TestConf] = takeTestConfsFromGroups(
      tscTestGroups.map { tg =>
        val proportion = tg.proportionPercents / 100.0
        (tg.id, Math.ceil(testsAmount * proportion).toInt)
      }
    )

    testSetTestsFromGroups
      .map(t => UserExamStepAttemptTestSetTest(-1, newTestSet.id, t.id))
      .foreach(createUserExamTestSetTest)

    (newTestSet, testSetTestsFromGroups)
  }

  def verifyTestSetTestAnswer(stepAttemptTestSetId: Long,
                              testAnswer: TestAnswerDto): VerifiedTestAnswerDto = {
    val testId = testAnswer.testId
    val testSetTest = getTestSetTest(stepAttemptTestSetId, testId)
    val verifiedTestAnswer = verifyTestAnswer(testAnswer)
    //Update information about test submission
    if(verifiedTestAnswer.isCorrectAnswer) {
      updateTestSetTest(testSetTest.copy(done = true))
    } else {
      updateTestSetTest(testSetTest.copy(mistakes = testSetTest.mistakes + verifiedTestAnswer.mistakesAmount))
    }
    verifiedTestAnswer
  }

  def verifyTestAnswer(testAnswer: TestAnswerDto): VerifiedTestAnswerDto = {
    val testConf = getTestConf(testAnswer.testId)
    val correctOptions = testConf.options.filter(_.correct)
    TestUtils.verify(testAnswer, correctOptions.map(_.id))
  }

  private def testToDto(t: TestConf): TestDto =
    TestDto(t.id, t.groupId, t.testType, t.question, t.imageUrl, t.help, t.options.map(new TestOptionDto(_)))

}

object TestSetQueries {
  import io.circe.parser._
  import io.circe.syntax._
  import io.circe.generic.auto._
  import edu.knuca.resmat.http.JsonProtocol._
  import anorm.SqlParser.{int, long, str, bool}

  object TS {
    val table = "test_set_confs"
    val id = "id"
    val name = "name"
    val maxTestsAmount = "max_tests_amount"
  }

  object TG {
    val table = "test_group_confs"
    val id = "id"
    val name = "name"
  }

  object TSTG {
    val table = "test_set_conf_test_group_confs"
    val id = "id"
    val testSetConfId = "test_set_conf_id"
    val testGroupConfId = "test_group_conf_id"
    val proportionPercents = "proportion_percents"
  }

  object T {
    val table = "test_confs"
    val id = "id"
    val groupConfId = "group_conf_id"
    val question = "question"
    val imageUrl = "image_url"
    val options = "options"
    val testType = "test_type"
    val help = "help"
  }

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
    val done = "done"
    val mistakes = "mistakes"
  }

  val tscParser  = for {
    id <- long(TS.id)
    name <- str(TS.name)
    maxTestsAmount <- int(TS.maxTestsAmount)
  } yield TestSetConf(id, name, maxTestsAmount)

  val tgcParser  = for {
    id <- long(TG.id)
    name <- str(TG.name)
  } yield TestGroupConf(id, name)

  val tsctgcParser  = for {
    id <- long(TG.id)
    testSetConfId <- long(TSTG.testSetConfId)
    testGroupConfId <- long(TSTG.testGroupConfId)
    proportionPercents <- int(TSTG.proportionPercents)
  } yield TestSetConfTestGroup(id, testSetConfId, testGroupConfId, proportionPercents)

  val tcParser  = for {
    id <- long(T.id)
    groupConfId <- int(T.groupConfId)
    question <- str(T.question)
    imageUrl <- str(T.imageUrl).?
    options <- str(T.options)
    testType <- int(T.testType)
    help <- str(T.help).?
  } yield TestConf(id, groupConfId, question, imageUrl, decodeTestOptions(options), TestType(testType), help)

  val uetsParser  = for {
    id <- long(UETS.id)
    stepAttemptId <- long(UETS.stepAttemptId)
    testSetConfId <- long(UETS.testSetConfId)
  } yield UserExamStepAttemptTestSet(id, stepAttemptId, testSetConfId)

  val uetstParser  = for {
    id <- long(UETST.id)
    stepAttemptTestSetId <- long(UETST.stepAttemptTestSetId)
    testConfId <- long(UETST.testConfId)
    done <- bool(UETST.done)
    mistakes <- int(UETST.mistakes)
  } yield UserExamStepAttemptTestSetTest(id, stepAttemptTestSetId, testConfId, done, mistakes)
  
  def createTestSetConf(tsc: TestSetConf) =
    SQL(s"INSERT INTO ${TS.table} (${TS.name}, ${TS.maxTestsAmount}) VALUES ({name}, {maxTestsAmount})")
      .on("name" -> tsc.name)
      .on("maxTestsAmount" -> tsc.maxTestsAmount)

  def createTestGroupConf(tsc: TestGroupConf) =
    SQL(s"INSERT INTO ${TG.table} (${TG.name}) VALUES ({name})").on("name" -> tsc.name)

  def createTestSetConfTestGroup(tsctg: TestSetConfTestGroup) =
    SQL(
      s"""INSERT INTO ${TSTG.table} (
         |${TSTG.testSetConfId}, 
         |${TSTG.testGroupConfId},
         |${TSTG.proportionPercents}
         |) VALUES (
         |{testSetConfId}, 
         |{testGroupConfId},
         |{proportionPercents}
         |)""".stripMargin)
      .on("testSetConfId" -> tsctg.testSetConfId)
      .on("testGroupConfId" -> tsctg.testGroupConfId)
      .on("proportionPercents" -> tsctg.proportionPercents)

  def createTestConf(tc: TestConf) =
    SQL(
      s"""INSERT INTO ${T.table} (
         |${T.groupConfId},
         |${T.question},
         |${T.imageUrl},
         |${T.options},
         |${T.testType},
         |${T.help}
         |) VALUES (
         |{groupConfId},
         |{question},
         |{imageUrl},
         |{options},
         |{testType},
         |{help}
         |)""".stripMargin)
    .on("groupConfId" -> tc.groupId)
    .on("question" -> tc.question)
    .on("imageUrl" -> tc.imageUrl)
    .on("options" -> tc.options.asJson.toString)
    .on("testType" -> tc.testType.id)
    .on("help" -> tc.help)

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
         |${UETST.done},
         |${UETST.mistakes}
         |) VALUES (
         |{stepAttemptTestSetId},
         |{testConfId},
         |{done},
         |{mistakes}
         |)""".stripMargin)
      .on("stepAttemptTestSetId" -> uetst.stepAttemptTestSetId)
      .on("testConfId" -> uetst.testConfId)
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

  def getTestSetConf(id: Long) = getById(TS.table, id)

  def getTestGroupConf(id: Long) = getById(TG.table, id)

  def getTestSetConfTestGroup(id: Long) = getById(TSTG.table, id)

  def findTestSetConfGroups(testSetConfId: Long) =
    SQL(s"SELECT * FROM ${TSTG.table} WHERE ${TSTG.testSetConfId} = {testSetConfId}").on("testSetConfId" -> testSetConfId)

  def getTestConf(id: Long) = getById(T.table, id)

  def findTestConfs(ids: Seq[Long]) =
    SQL(s"SELECT * FROM ${T.table} WHERE ${T.id} IN ({ids})").on("ids" -> ids)

  def findStepAttemptTestsByStepAttemptTestSetId(stepAttemptTestSetId: Long) =
    SQL(s"SELECT * FROM ${UETST.table} WHERE ${UETST.stepAttemptTestSetId} = {stepAttemptTestSetId}")
      .on("stepAttemptTestSetId" -> stepAttemptTestSetId)

  def getStepAttemptTest(stepAttemptTestSetId: Long, testConfId: Long) =
    SQL(s"SELECT * FROM ${UETST.table} WHERE ${UETST.stepAttemptTestSetId} = {stepAttemptTestSetId} AND ${UETST.testConfId} = {testConfId}")
      .on("stepAttemptTestSetId" -> stepAttemptTestSetId)
      .on("testConfId" -> testConfId)

  def findTestConfsByGroup(groupConfId: Long) =
    SQL(s"SELECT * FROM ${T.table} WHERE ${T.groupConfId} = {groupConfId}").on("groupConfId" -> groupConfId)

  def getUserExamTestSet(id: Long) = getById(UETS.table, id)

  def getUserExamTestSetTest(id: Long) = getById(UETST.table, id)

  def getUserExamTestSetByAttemptId(stepAttemptId: Long) =
    SQL(s"SELECT * FROM ${UETS.table} WHERE ${UETS.stepAttemptId} = {stepAttemptId}").on("stepAttemptId" -> stepAttemptId)

  private def getById(tableName: String, id: Long) = SQL(s"SELECT * FROM $tableName WHERE id = {id}").on("id" -> id)

  private def decodeTestOptions(json: String): Seq[TestOptionConf] = decode[Seq[TestOptionConf]](json).fold( e =>
    throw new RuntimeException(s"Failed to decode TestOptionConf in json: $json", e),
    r => r
  )
}