package edu.knuca.resmat.tests

import java.sql.Connection

import anorm.{BatchSql, NamedParameter, SQL}
import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam._
import edu.knuca.resmat.user.AuthenticatedUser
import edu.knuca.resmat.utils.{CollectionUtils, S3Manager, SqlUtils}

import scala.concurrent.{Await, ExecutionContext, Future}

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

  def createTestGroupConf(testGroupConf: TestGroupConf, ownerUserIdOpt: Option[Long] = None): TestGroupConf = {
    val id = db.runTransaction { implicit c =>
      val insertedIdOpt: Option[Long] = Q.createTestGroupConf(testGroupConf).executeInsert()
      val insertedId = insertedIdOpt.getOrElse(
        throw new RuntimeException(s"Failed to create $testGroupConf")
      )
      ownerUserIdOpt.foreach(ownerUserId => {
        Q.grantAccessToTestGroups(ownerUserId, Set(insertedId)).execute()
      })
      insertedId
    }
    getTestGroupConf(id)
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

  def getTestGroupConfs(isArchived: Option[Boolean], onlyAccessible: Boolean = false)(implicit user: AuthenticatedUser): Seq[TestGroupConf] = db.run { implicit c =>
    val accessibleForUserId = if (onlyAccessible) { Some(user.id) } else None
    Q.getTestGroupConfs(isArchived, accessibleForUserId).as(Q.tgcParser.*)
  }

  def getTestGroupConfsWithAmountOfTests(): Seq[TestGroupConfWithAmountOfTestsDto] = db.run { implicit c =>
    Q.getTestGroupConfsWithAmountOfTests.as(Q.tgcWithAmountOftestsParser.*)
  }

  def getAccessToTestGroups(userId: Long): UserTestGroupAccessDto = db.run{ implicit c =>
    val sgIds = Q.getAccessToTestGroups(userId).as(Q.userTestGroupAccessParser.*)
    UserTestGroupAccessDto(userId, sgIds.toSet)
  }

  def setUserTestGroupAccess(dto: UserTestGroupAccessDto): Unit = db.runTransaction { implicit c =>
    Q.removeAccessToTestGroups(dto.userId).execute()
    if (dto.testGroupIds.nonEmpty) {
      Q.grantAccessToTestGroups(dto.userId, dto.testGroupIds).execute()
    }
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
    val id = db.run { implicit c =>
      createTestConfInternal(testConf)
    }
    getTestConf(id)
  }

  private def createTestConfInternal(testConf: TestConf)(implicit c: Connection): Long = {
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

  def updateTestConf(id: Long, testConf: TestConf): TestConf = db.run { implicit c =>
    updateTestConfInternal(id, testConf)
    getTestConf(id)
  }

  private def updateTestConfInternal(id: Long, testConf: TestConf)(implicit c: Connection): Unit = {
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
  }

  def deleteTestConf(id: Long): Unit = db.run { implicit c =>
    deleteTestConfInternal(id)
  }

  def deleteTestConfInternal(id: Long)(implicit c: Connection): Unit = {
    val updatedRows = Q.deleteTestConf(id).executeUpdate()
    if(updatedRows == 0) {
      throw new RuntimeException(s"Failed to delete test conf with id $id")
    }
    if(updatedRows > 1) {
      throw new RuntimeException(s"Deleted $updatedRows rows while deleting test conf by id $id")
    }
  }
  
  def bulkSetGroupTestConfs(groupId: Long, testConfs: Seq[TestConf]): Seq[TestConf] = {
    db.runTransaction { implicit c =>
      val existingTestConfs = findTestConfsByGroup(groupId)
      val diff = CollectionUtils.diff[TestConf, TestConf](existingTestConfs, testConfs, (tc1, tc2) => tc1.id == tc2.id)
      val addedFuture = Future.sequence(
        diff.added.map(tc =>
          Future(createTestConfInternal(tc))
        )
      )
      val updated = diff.same.filterNot(tc => existingTestConfs.contains(tc))
      val updatedFuture = Future.sequence(
        updated.map(tc =>
          Future(updateTestConfInternal(tc.id, tc))
        )
      )
      val removedFuture = Future.sequence(
        diff.removed.map(tc =>
          Future(deleteTestConfInternal(tc.id))
        )
      )
      val resultFuture = for {
        _ <- addedFuture
        _ <- updatedFuture
        _ <- removedFuture
      } yield ()
      import scala.concurrent.duration._
      Await.result(resultFuture, 30 seconds)
    }
    findTestConfsByGroup(groupId)
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

  def takeTestConfsFromGroups(testGroupConfs: Seq[(TestSetConfTestGroup, Int)]): Seq[(TestSetConfTestGroup, Seq[TestConf])] =
    testGroupConfs.map{ case(g, proportion) =>
      val groupTests = findTestConfsByGroup(g.testGroupConfId)
      val withProportion = scala.util.Random.shuffle(groupTests).take(proportion)
      (g, withProportion)
    }
}

object TestConfsQueries {
  import io.circe.parser._
  import io.circe.syntax._
  import io.circe.generic.auto._
  import edu.knuca.resmat.http.JsonProtocol._

  import anorm.SqlParser.{int, long, str, double, bool}

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
    val isArchived = "is_archived"
  }

  object TSCTGC {
    val table = "test_set_conf_test_group_confs"
    val id = "id"
    val testSetConfId = "test_set_conf_id"
    val testGroupConfId = "test_group_conf_id"
    val proportionPercents = "proportion_percents"
    val mistakeValue = "mistake_value"
  }

  object T {
    val table = "test_confs"
    val id = "id"
    val groupConfId = "group_conf_id"
    val question = "question"
    val imageUrl = "image_url"
    val options = "options"
    val sequence = "sequence"
    val testType = "test_type"
    val precision = "calculation_precision"
    val help = "help"
  }

  object UsTGs {
    val table = "users_test_groups"
    val userId = "user_id"
    val testGroupId = "test_group_id"
  }
  val userTestGroupAccessParser = for {
    testGroupId <- long(UsTGs.testGroupId)
  } yield testGroupId

  val tscParser  = for {
    id <- long(TSC.id)
    name <- str(TSC.name)
    maxTestsAmount <- int(TSC.maxTestsAmount)
  } yield TestSetConf(id, name, maxTestsAmount)

  val tgcParser  = for {
    id <- long(TG.id)
    parentGroupId <- long(TG.parentGroupId).?
    name <- str(TG.name)
    isArchived <- bool(TG.isArchived)
  } yield TestGroupConf(id, name, isArchived, parentGroupId)

  val tgcWithAmountOftestsParser  = for {
    id <- long(TG.id)
    parentGroupId <- long(TG.parentGroupId).?
    name <- str(TG.name)
    isArchived <- bool(TG.isArchived)
    amountOfTests <- int("amountOfTests")
  } yield TestGroupConfWithAmountOfTestsDto(TestGroupConf(id, name, isArchived, parentGroupId), amountOfTests)

  val tsctgcParser  = for {
    id <- long(TG.id)
    testSetConfId <- long(TSCTGC.testSetConfId)
    testGroupConfId <- long(TSCTGC.testGroupConfId)
    proportionPercents <- int(TSCTGC.proportionPercents)
    mistakeValue <- double(TSCTGC.mistakeValue).?
  } yield TestSetConfTestGroup(id, testSetConfId, testGroupConfId, proportionPercents, mistakeValue)

  def tcParser(s3Manager: S3Manager)  = for {
    id <- long(T.id)
    groupConfId <- int(T.groupConfId)
    question <- str(T.question)
    imageUrl <- str(T.imageUrl).?
    options <- str(T.options)
    testType <- int(T.testType)
    help <- str(T.help).?
    precision <- double(T.precision).?
    sequence <- int(T.sequence)
  } yield updateS3KeysToUrls(
    TestConf(id, groupConfId, question, imageUrl, decodeTestOptions(options), TestType(testType), help, precision, sequence),
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
    SQL(s"UPDATE ${TG.table} SET ${TG.name} = {name}, ${TG.isArchived} = {isArchived}, ${TG.parentGroupId} = {parentGroupId} WHERE ${TG.id} = {id}")
      .on("id" -> id)
      .on("name" -> tsc.name)
      .on("isArchived" -> tsc.isArchived)
      .on("parentGroupId" -> tsc.parentGroupId.map(java.lang.Long.valueOf(_)).orNull)

  def getAccessToTestGroups(userId: Long) = {
    SQL(s"SELECT * FROM ${UsTGs.table} WHERE ${UsTGs.userId} = {userId}")
      .on("userId" -> userId)
  }
  def grantAccessToTestGroups(userId: Long, tgIds: Set[Long]): BatchSql = {
    val values = tgIds.toSeq.map( ecId =>
      Seq[NamedParameter](
        "userId" -> userId,
        "testGroupId" -> ecId
      )
    )
    BatchSql(
      s"""INSERT INTO ${UsTGs.table} (
         |${UsTGs.userId},
         |${UsTGs.testGroupId}
         |) VALUES (
         |{userId},
         |{testGroupId}
         |)""".stripMargin,
      values.head,
      values.tail : _*
    )
  }
  def removeAccessToTestGroups(userId: Long) = {
    SQL(s"DELETE FROM ${UsTGs.table} WHERE ${UsTGs.userId} = {userId}")
      .on("userId" -> userId)
  }

  def createTestSetConfTestGroup(tsctg: TestSetConfTestGroup) =
    SQL(
      s"""INSERT INTO ${TSCTGC.table} (
         |${TSCTGC.testSetConfId},
         |${TSCTGC.testGroupConfId},
         |${TSCTGC.proportionPercents},
         |${TSCTGC.mistakeValue}
         |) VALUES (
         |{testSetConfId},
         |{testGroupConfId},
         |{proportionPercents},
         |{mistakeValue}
         |)""".stripMargin)
      .on("testSetConfId" -> tsctg.testSetConfId)
      .on("testGroupConfId" -> tsctg.testGroupConfId)
      .on("proportionPercents" -> tsctg.proportionPercents)
      .on("mistakeValue" -> tsctg.mistakeValue)

  def createTestSetConfTestGroups(tsctgs: Seq[TestSetConfTestGroup]): BatchSql = {
    val values = tsctgs.map( tg =>
      Seq[NamedParameter](
        "testSetConfId" -> tg.testSetConfId,
        "testGroupConfId" -> tg.testGroupConfId,
        "proportionPercents" -> tg.proportionPercents,
        "mistakeValue" -> tg.mistakeValue
      )
    )
    BatchSql(
      s"""INSERT INTO ${TSCTGC.table} (
         |${TSCTGC.testSetConfId},
         |${TSCTGC.testGroupConfId},
         |${TSCTGC.proportionPercents},
         |${TSCTGC.mistakeValue}
         |) VALUES (
         |{testSetConfId},
         |{testGroupConfId},
         |{proportionPercents},
         |{mistakeValue}
         |)""".stripMargin,
      values.head,
      values.tail : _*
    )
  }

  def deleteTestSetConfTestGroupsByTestSetConfId(testSetConfId: Long) =
    SQL(s"DELETE FROM ${TSCTGC.table} WHERE ${TSCTGC.testSetConfId} = {testSetConfId}")
      .on("testSetConfId" -> testSetConfId)

  def createTestConf(tc: TestConf) =
    SQL(
      s"""INSERT INTO ${T.table} (
         |${T.groupConfId},
         |${T.question},
         |${T.imageUrl},
         |${T.options},
         |${T.testType},
         |${T.help},
         |${T.precision},
         |${T.sequence}
         |) VALUES (
         |{groupConfId},
         |{question},
         |{imageUrl},
         |{options},
         |{testType},
         |{help},
         |{precision},
         |{sequence}
         |)""".stripMargin)
      .on("groupConfId" -> tc.groupId)
      .on("question" -> tc.question)
      .on("imageUrl" -> tc.imageUrl)
      .on("options" -> tc.options.asJson.toString)
      .on("testType" -> tc.testType.id)
      .on("help" -> tc.help)
      .on("precision" -> tc.precision)
      .on("sequence" -> tc.sequence)

  def updateTestConf(id: Long, tc: TestConf) =
    SQL(
      s"""UPDATE ${T.table} SET
         |${T.groupConfId} = {groupConfId},
         |${T.question} = {question},
         |${T.imageUrl} = {imageUrl},
         |${T.options} = {options},
         |${T.testType} = {testType},
         |${T.help} = {help},
         |${T.precision} = {precision},
         |${T.sequence} = {sequence}
         |WHERE ${T.id} = {id}
         |""".stripMargin)
      .on("id" -> id)
      .on("groupConfId" -> tc.groupId)
      .on("question" -> tc.question)
      .on("imageUrl" -> tc.imageUrl)
      .on("options" -> tc.options.asJson.toString)
      .on("testType" -> tc.testType.id)
      .on("help" -> tc.help)
      .on("precision" -> tc.precision)
      .on("sequence" -> tc.sequence)

  def deleteTestConf(testConfId: Long) =
    SQL(s"DELETE FROM ${T.table} WHERE ${T.id} = {id}").on("id" -> testConfId)

  def getTestSetConf(id: Long) = SqlUtils.get(TSC.table, id)

  def getTestGroupConf(id: Long) = SqlUtils.get(TG.table, id)

  def getTestGroupConfs(isArchived: Option[Boolean], accessibleForUserId: Option[Long]) = {
    val archivedCondition = isArchived
      .map(ia => s"WHERE ${TG.isArchived} IS $ia")
      .getOrElse(s"WHERE ${TG.isArchived} IS FALSE")
    val accessibleJoin = accessibleForUserId
      .map(userId => s"JOIN ${UsTGs.table} ON ${UsTGs.table}.${UsTGs.testGroupId} = ${TG.table}.${TG.id} AND ${UsTGs.table}.${UsTGs.userId} = $userId")
      .getOrElse("")
    SQL(
      s"SELECT ${TG.table}.* FROM ${TG.table} $accessibleJoin $archivedCondition"
    )
  }

  def getTestGroupConfsWithAmountOfTests =
    SQL(
      s""" SELECT tg.*, COUNT(t.id) amountOfTests FROM ${TG.table} tg
         | LEFT JOIN ${T.table} t ON t.${T.groupConfId} = tg.${TG.id}
         | GROUP BY tg.id
       """.stripMargin
    )

  def getTestSetConfTestGroup(id: Long) = SqlUtils.get(TSCTGC.table, id)

  def findTestSetConfGroups(testSetConfId: Long) =
    SQL(s"SELECT * FROM ${TSCTGC.table} WHERE ${TSCTGC.testSetConfId} = {testSetConfId}").on("testSetConfId" -> testSetConfId)

  def getTestConf(id: Long) = SqlUtils.get(T.table, id)

  def findTestConfs(ids: Seq[Long]) =
    SQL(s"SELECT * FROM ${T.table} WHERE ${T.id} IN ({ids})").on("ids" -> ids)

  def findTestConfsByGroup(groupConfId: Long) =
    SQL(s"SELECT * FROM ${T.table} WHERE ${T.groupConfId} = {groupConfId} ORDER BY ${T.sequence}")
      .on("groupConfId" -> groupConfId)

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
