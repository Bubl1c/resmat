package edu.knuca.resmat.data

import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.testset.TestSetExamService

object TestSetData {
  val testSetConfs: List[TestSetConf] = List(
    TestSetConf(1, "Набір тестів для крутих студентів", 3)
  )

  val testGroupConfs: List[(TestGroupConf, Seq[TestConf])] = List(
    (TestGroupConf(1, "Група тестів 1"), Seq(
      TestConf(-1, -1, "Test11 {sigma}", Some("img/tasks/9.png"), Seq(
        TestOptionConf(1, "{theta}Option1", true),
        TestOptionConf(2, "<br/>\n    w(a) = ? m <br/>\n    {phi}(a) = ? рад <br/>\n    Mr(а) = 0 кНм / м <br/>\n    Qr (а) = 0 кН / м <br/>\n    w(b) = 0 м<br/>\n    {phi}(b) = ? рад<br/>\n    Mr(b) = 0 кНм / м<br/>\n    Qr(b) = ? кН / м<br/><br/>"),
        TestOptionConf(3, "Option3"),
        TestOptionConf(4, "Option4")
      )),
      TestConf(-1, -1, "Test12", None, Seq(
        TestOptionConf(1, "Option1", true)
      ))
    )),
    (TestGroupConf(2, "Група тестів 2"), Seq(
      TestConf(-1, -1, "Test21", None, Seq(
        TestOptionConf(1, "Option1", true),
        TestOptionConf(2, "Option2")
      ), TestType.Checkbox),
      TestConf(-1, -1, "Test22", None, Seq(
        TestOptionConf(1, "Option1", true)
      ))
    )),
    (TestGroupConf(3, "Група тестів 3"), Seq(
      TestConf(-1, -1, "Test31", None, Seq(
        TestOptionConf(1, "Option1", true)
      )),
      TestConf(-1, -1, "Test32", None, Seq(
        TestOptionConf(1, "Option1", true)
      ))
    ))
  )
}

class TestSetDataGenerator(testSetExamService: TestSetExamService) {

  val testSetConfs: Seq[TestSetConf] = TestSetData.testSetConfs.map(testSetExamService.createTestSetConf)

  private val groupsWithTests = generateGroupsWithTests

  val groupConfs: Seq[TestGroupConf] = groupsWithTests.map(_._1)
  val testConfs: Seq[TestConf] = groupsWithTests.flatMap(_._2)

  val testSetConfGroups: Seq[TestSetConfTestGroup] = addGroupsToTestSet(testSetConfs.head, groupConfs)

  def generateGroupsWithTests: Seq[(TestGroupConf, Seq[TestConf])] = {
    TestSetData.testGroupConfs.map{ case(tgConf, tConfs) =>
      val tgc = testSetExamService.createTestGroupConf(tgConf)
      val testConfs = tConfs.map(tc =>
        testSetExamService.createTestConf(tc.copy(groupId = tgc.id))
      )
      (tgc, testConfs)
    }
  }

  def addGroupsToTestSet(testSetConf: TestSetConf, groups: Seq[TestGroupConf]): Seq[TestSetConfTestGroup] = {
    val balancedProportion = 100 / groups.size
    groups.map(gc =>
      testSetExamService.createTestSetConfTestGroup(TestSetConfTestGroup(-1, testSetConf.id, gc.id, balancedProportion))
    )
  }
}
