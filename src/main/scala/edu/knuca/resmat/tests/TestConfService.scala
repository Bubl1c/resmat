package edu.knuca.resmat.tests

import java.sql.Connection

import anorm.{BatchSql, NamedParameter, SQL}
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam._
import edu.knuca.resmat.utils.{S3Manager, SqlUtils}

import scala.concurrent.ExecutionContext

class TestConfService(val db: DatabaseService, s3Manager: S3Manager)
                     (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.tests.{TestConfsQueries => Q}

  //====================TestSetConf====================

  def createTestSetConf(testSetConf: TestSetConf): TestSetConf = {
    val insertedId = db.run { implicit c =>
      createTestSetConfTransact(testSetConf)
    }
    getTestSetConf(insertedId)
  }

  def createTestSetConfTransact(testSetConf: TestSetConf)(implicit c: Connection): Long = {
    val insertedIdOpt: Option[Long] = Q.createTestSetConf(testSetConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $testSetConf")
    )
    insertedId
  }

  def updateTestSetConfTransact(id: Long, testSetConf: TestSetConf)(implicit c: Connection): Unit = {
    val updatedRows: Int = Q.updateTestSetConf(id, testSetConf).executeUpdate()
    if(updatedRows == 0) {
      throw new RuntimeException(s"Failed to update $testSetConf by id $id")
    }
    if(updatedRows > 1) {
      throw new RuntimeException(s"Updated $updatedRows rows while updating $testSetConf by id $id")
    }
  }

  def deleteTestSetConfTransact(id: Long)(implicit c: Connection): Unit = {
    val updatedRows: Int = Q.deleteTestSetConf(id).executeUpdate()
    if(updatedRows == 0) {
      throw new RuntimeException(s"Failed to delete test set conf by id $id")
    }
    if(updatedRows > 1) {
      throw new RuntimeException(s"Deleted $updatedRows rows while deleting test set conf by id $id")
    }
  }

  def getTestSetConf(id: Long): TestSetConf = db.run { implicit c =>
    Q.getTestSetConf(id).as(Q.tscParser.singleOpt).getOrElse(
      throw new RuntimeException(s"Test set conf with id $id not found")
    )
  }

  def getTestSetConfDto(testSetConfId: Long): TestSetConfDto = {
    val tsc = getTestSetConf(testSetConfId)
    val testGroups = findTestSetConfGroups(testSetConfId)
    TestSetConfDto(tsc, testGroups)
  }

  //====================TestGroupConf====================

  def createTestGroupConf(testGroupConf: TestGroupConf): TestGroupConf = db.run { implicit c =>
    val insertedIdOpt: Option[Long] = Q.createTestGroupConf(testGroupConf).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $testGroupConf")
    )
    getTestGroupConf(insertedId)
  }

  def updateTestGroupConf(id: Long, testGroupConf: TestGroupConf) = db.run { implicit c =>
    Q.updateTestGroupConf(id, testGroupConf).executeUpdate()
    getTestGroupConf(id)
  }

  def getTestGroupConf(id: Long): TestGroupConf = db.run { implicit c =>
    Q.getTestGroupConf(id).as(Q.tgcParser.singleOpt).getOrElse(
      throw new RuntimeException(s"TestGroupConf with id $id not found")
    )
  }

  def getTestGroupConfs(): Seq[TestGroupConf] = db.run { implicit c =>
    Q.getTestGroupConfs.as(Q.tgcParser.*)
  }

  def getTestGroupConfsWithAmountOfTests(): Seq[TestGroupConfWithAmountOfTestsDto] = db.run { implicit c =>
    Q.getTestGroupConfsWithAmountOfTests.as(Q.tgcWithAmountOftestsParser.*)
  }

  //====================TestSetConfTestGroup====================

  def createTestSetConfTestGroup(tsctg: TestSetConfTestGroup): TestSetConfTestGroup = db.run { implicit c =>
    val insertedId = createTestSetConfTestGroupTransact(tsctg)
    getTestSetConfTestGroup(insertedId)
  }

  def createTestSetConfTestGroupTransact(tsctg: TestSetConfTestGroup)(implicit c: Connection): Long = {
    val insertedIdOpt: Option[Long] = Q.createTestSetConfTestGroup(tsctg).executeInsert()
    val insertedId = insertedIdOpt.getOrElse(
      throw new RuntimeException(s"Failed to create $tsctg")
    )
    insertedId
  }

  def createTestSetConfTestGroupsTransact(tsctgs: Seq[TestSetConfTestGroup])(implicit c: Connection): Unit = {
    if (tsctgs.nonEmpty) {
      Q.createTestSetConfTestGroups(tsctgs).execute()
    }
  }

  def deleteTestSetConfTestGroupsTransact(testSetConfId: Long)(implicit c: Connection): Unit = {
    Q.deleteTestSetConfTestGroupsByTestSetConfId(testSetConfId).executeUpdate()
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

  def createTestConf(testConf: TestConf): TestConf = {
    val id = db.runTransaction { implicit c =>
      val insertedIdOpt: Option[Long] = Q.createTestConf(testConf).executeInsert()
      val insertedId = insertedIdOpt.getOrElse(
        throw new RuntimeException(s"Failed to create $testConf")
      )
      Q.updateTestConf(
        insertedId,
        TestConfsQueries.updateUrlsToS3Keys(testConf.copy(id = insertedId), s3Manager)
      ).executeUpdate()
      insertedId
    }
    getTestConf(id)
  }

  def editTestConf(id: Long, testConf: TestConf): TestConf = db.run { implicit c =>
    val updatedRows: Int = Q.updateTestConf(
      id,
      TestConfsQueries.updateUrlsToS3Keys(testConf, s3Manager)
    ).executeUpdate()
    if(updatedRows == 0) {
      throw new RuntimeException(s"Failed to update $testConf by id $id")
    }
    if(updatedRows > 1) {
      throw new RuntimeException(s"Updated $updatedRows rows while updating $testConf by id $id")
    }
    getTestConf(id)
  }

  def deleteTestConf(id: Long): Unit = db.runTransaction { implicit c =>
    val updatedRows = Q.deleteTestConf(id).executeUpdate()
    if(updatedRows == 0) {
      throw new RuntimeException(s"Failed to delete test conf with id $id")
    }
    if(updatedRows > 1) {
      throw new RuntimeException(s"Deleted $updatedRows rows while deleting test conf by id $id")
    }
  }

  def getTestConf(id: Long): TestConf = db.run { implicit c =>
    Q.getTestConf(id).as(Q.tcParser(s3Manager).singleOpt).getOrElse(
      throw new RuntimeException(s"Test conf with id $id not found")
    )
  }

  def findTestConfs(ids: Seq[Long]): Seq[TestConf] = db.run { implicit c =>
    Q.findTestConfs(ids).as(Q.tcParser(s3Manager).*)
  }

  def findTestConfsByGroup(groupConfId: Long): Seq[TestConf] = db.run { implicit c =>
    Q.findTestConfsByGroup(groupConfId).as(Q.tcParser(s3Manager).*)
  }

  def takeTestConfsFromGroups(groupIdsWithProportions: Seq[(Long, Int)]): Seq[TestConf] =
    groupIdsWithProportions.flatMap{ case(groupId, proportion) =>
      val groupTests = findTestConfsByGroup(groupId)
      val withProportion = scala.util.Random.shuffle(groupTests).take(proportion)
      withProportion
    }
}

object TestConfsQueries {
  import io.circe.parser._
  import io.circe.syntax._
  import io.circe.generic.auto._
  import edu.knuca.resmat.http.JsonProtocol._

  import anorm.SqlParser.{int, long, str, bool, get}

  object TSC {
    val table = "test_set_confs"
    val id = "id"
    val name = "name"
    val maxTestsAmount = "max_tests_amount"
  }

  object TG {
    val table = "test_group_confs"
    val id = "id"
    val name = "name"
    val parentGroupId = "parent_group_id"
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

  val tscParser  = for {
    id <- long(TSC.id)
    name <- str(TSC.name)
    maxTestsAmount <- int(TSC.maxTestsAmount)
  } yield TestSetConf(id, name, maxTestsAmount)

  val tgcParser  = for {
    id <- long(TG.id)
    parentGroupId <- long(TG.parentGroupId).?
    name <- str(TG.name)
  } yield TestGroupConf(id, name, parentGroupId)

  val tgcWithAmountOftestsParser  = for {
    id <- long(TG.id)
    parentGroupId <- long(TG.parentGroupId).?
    name <- str(TG.name)
    amountOfTests <- int("amountOfTests")
  } yield TestGroupConfWithAmountOfTestsDto(TestGroupConf(id, name, parentGroupId), amountOfTests)

  val tsctgcParser  = for {
    id <- long(TG.id)
    testSetConfId <- long(TSTG.testSetConfId)
    testGroupConfId <- long(TSTG.testGroupConfId)
    proportionPercents <- int(TSTG.proportionPercents)
  } yield TestSetConfTestGroup(id, testSetConfId, testGroupConfId, proportionPercents)

  def tcParser(s3Manager: S3Manager)  = for {
    id <- long(T.id)
    groupConfId <- int(T.groupConfId)
    question <- str(T.question)
    imageUrl <- str(T.imageUrl).?
    options <- str(T.options)
    testType <- int(T.testType)
    help <- str(T.help).?
  } yield updateS3KeysToUrls(
    TestConf(id, groupConfId, question, imageUrl, decodeTestOptions(options), TestType(testType), help),
    s3Manager
  )

  def createTestSetConf(tsc: TestSetConf) =
    SQL(s"INSERT INTO ${TSC.table} (${TSC.name}, ${TSC.maxTestsAmount}) VALUES ({name}, {maxTestsAmount})")
      .on("name" -> tsc.name)
      .on("maxTestsAmount" -> tsc.maxTestsAmount)

  def updateTestSetConf(id: Long, tsc: TestSetConf) =
    SQL(s"UPDATE ${TSC.table} SET ${TSC.name} = {name}, ${TSC.maxTestsAmount} = {maxTestsAmount} WHERE ${TSC.id} = {id}")
      .on("id" -> id)
      .on("name" -> tsc.name)
      .on("maxTestsAmount" -> tsc.maxTestsAmount)

  def deleteTestSetConf(id: Long) =
    SQL(s"DELETE FROM ${TSC.table} WHERE ${TSC.id} = {id}")
      .on("id" -> id)

  def createTestGroupConf(tsc: TestGroupConf) = {
    SQL(s"INSERT INTO ${TG.table} (${TG.name}, ${TG.parentGroupId}) VALUES ({name}, {parentGroupId})")
      .on("name" -> tsc.name)
      .on("parentGroupId" -> tsc.parentGroupId.map(java.lang.Long.valueOf(_)).orNull)
  }

  def updateTestGroupConf(id: Long, tsc: TestGroupConf) =
    SQL(s"UPDATE ${TG.table} SET ${TG.name} = {name}, ${TG.parentGroupId} = {parentGroupId} WHERE ${TG.id} = {id}")
      .on("id" -> id)
      .on("name" -> tsc.name)
      .on("parentGroupId" -> tsc.parentGroupId.map(java.lang.Long.valueOf(_)).orNull)

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

  def createTestSetConfTestGroups(tsctgs: Seq[TestSetConfTestGroup]): BatchSql = {
    val values = tsctgs.map( tg =>
      Seq[NamedParameter](
        "testSetConfId" -> tg.testSetConfId,
        "testGroupConfId" -> tg.testGroupConfId,
        "proportionPercents" -> tg.proportionPercents
      )
    )
    BatchSql(
      s"""INSERT INTO ${TSTG.table} (
         |${TSTG.testSetConfId},
         |${TSTG.testGroupConfId},
         |${TSTG.proportionPercents}
         |) VALUES (
         |{testSetConfId},
         |{testGroupConfId},
         |{proportionPercents}
         |)""".stripMargin,
      values.head,
      values.tail : _*
    )
  }

  def deleteTestSetConfTestGroupsByTestSetConfId(testSetConfId: Long) =
    SQL(s"DELETE FROM ${TSTG.table} WHERE ${TSTG.testSetConfId} = {testSetConfId}")
      .on("testSetConfId" -> testSetConfId)

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

  def updateTestConf(id: Long, tc: TestConf) =
    SQL(
      s"""UPDATE ${T.table} SET
         |${T.groupConfId} = {groupConfId},
         |${T.question} = {question},
         |${T.imageUrl} = {imageUrl},
         |${T.options} = {options},
         |${T.testType} = {testType},
         |${T.help} = {help}
         |WHERE ${T.id} = {id}
         |""".stripMargin)
      .on("id" -> id)
      .on("groupConfId" -> tc.groupId)
      .on("question" -> tc.question)
      .on("imageUrl" -> tc.imageUrl)
      .on("options" -> tc.options.asJson.toString)
      .on("testType" -> tc.testType.id)
      .on("help" -> tc.help)

  def deleteTestConf(testConfId: Long) =
    SQL(s"DELETE FROM ${T.table} WHERE ${T.id} = {id}").on("id" -> testConfId)

  def getTestSetConf(id: Long) = SqlUtils.get(TSC.table, id)

  def getTestGroupConf(id: Long) = SqlUtils.get(TG.table, id)

  def getTestGroupConfs = SqlUtils.get(TG.table)

  def getTestGroupConfsWithAmountOfTests =
    SQL(
      s""" SELECT tg.*, COUNT(t.id) amountOfTests FROM ${TG.table} tg
         | LEFT JOIN ${T.table} t ON t.${T.groupConfId} = tg.${TG.id}
         | GROUP BY tg.id
       """.stripMargin
    )

  def getTestSetConfTestGroup(id: Long) = SqlUtils.get(TSTG.table, id)

  def findTestSetConfGroups(testSetConfId: Long) =
    SQL(s"SELECT * FROM ${TSTG.table} WHERE ${TSTG.testSetConfId} = {testSetConfId}").on("testSetConfId" -> testSetConfId)

  def getTestConf(id: Long) = SqlUtils.get(T.table, id)

  def findTestConfs(ids: Seq[Long]) =
    SQL(s"SELECT * FROM ${T.table} WHERE ${T.id} IN ({ids})").on("ids" -> ids)

  def findTestConfsByGroup(groupConfId: Long) =
    SQL(s"SELECT * FROM ${T.table} WHERE ${T.groupConfId} = {groupConfId}").on("groupConfId" -> groupConfId)

  private def decodeTestOptions(json: String): Seq[TestOptionConf] = decode[Seq[TestOptionConf]](json).fold( e =>
    throw new RuntimeException(s"Failed to decode TestOptionConf in json: $json", e),
    r => r
  )

  private def s3TestFolder(testId: Long) = s"test-confs/$testId"

  def updateS3KeysToUrls(testConf: TestConf, s3Manager: S3Manager): TestConf = {
    testConf.copy(
      imageUrl = testConf.imageUrl.map(s3Manager.urlFromKey),
      help = testConf.help.map(s3Manager.urlFromKey),
      options = testConf.options.map(option =>
        if(option.valueType == TestOptionValueType.Image) {
          option.copy(value = s3Manager.urlFromKey(option.value))
        } else {
          option
        }
      )
    )
  }

  def updateUrlsToS3Keys(testConf: TestConf, s3Manager: S3Manager): TestConf = {
    val testFolder = s3TestFolder(testConf.id)
    testConf.copy(
      imageUrl = testConf.imageUrl.map(s3Manager.urlToS3Key(_, testFolder)),
      help = testConf.help.map(s3Manager.urlToS3Key(_, testFolder)),
      options = testConf.options.map(option =>
        if(option.valueType == TestOptionValueType.Image) {
          option.copy(value = s3Manager.urlToS3Key(option.value, testFolder))
        } else {
          option
        }
      )
    )
  }
}
