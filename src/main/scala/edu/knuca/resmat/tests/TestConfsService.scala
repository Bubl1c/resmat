package edu.knuca.resmat.tests

import anorm.SQL
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam._
import edu.knuca.resmat.utils.SqlUtils

import scala.concurrent.ExecutionContext

class TestConfsService (val db: DatabaseService)
                       (implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.tests.{TestConfsQueries => Q}

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

  def getTestGroupConfs(): Seq[TestGroupConf] = db.run { implicit c =>
    Q.getTestGroupConfs.as(Q.tgcParser.*)
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

  def findTestConfsByGroup(groupId: Long): Seq[TestConf] = db.run { implicit c =>
    Q.findTestConfsByGroup(groupId).as(Q.tcParser.*)
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

  def getTestSetConf(id: Long) = SqlUtils.get(TS.table, id)

  def getTestGroupConf(id: Long) = SqlUtils.get(TG.table, id)

  def getTestGroupConfs = SqlUtils.get(TG.table)

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
}
