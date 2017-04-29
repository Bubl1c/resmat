package edu.knuca.resmat.data

import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.testset.TestSetExamService

object TestSetData {
  val testSetConfs: List[TestSetConf] = List(
    TestSetConf(1, "Набір тестів для крутих студентів", 20)
  )

  val testGroupConfs: List[(TestGroupConf, Seq[TestConf])] = List(
    (TestGroupConf(1, "Група тестів 1"), Seq(
      TestConf(-1, -1, "Test11", Seq(
        TestOptionConf(1, "Option1", true),
        TestOptionConf(2, "Option2"),
        TestOptionConf(3, "Option3"),
        TestOptionConf(4, "Option4")
      )),
      TestConf(-1, -1, "Test12", Seq(
        TestOptionConf(1, "Option1", true)
      ))
    )),
    (TestGroupConf(2, "Група тестів 2"), Seq(
      TestConf(-1, -1, "Test21", Seq(
        TestOptionConf(1, "Option1", true),
        TestOptionConf(2, "Option2")
      ), TestType.Checkbox),
      TestConf(-1, -1, "Test22", Seq(
        TestOptionConf(1, "Option1", true)
      ))
    )),
    (TestGroupConf(3, "Група тестів 3"), Seq(
      TestConf(-1, -1, "Test31", Seq(
        TestOptionConf(1, "Option1", true)
      )),
      TestConf(-1, -1, "Test32", Seq(
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
