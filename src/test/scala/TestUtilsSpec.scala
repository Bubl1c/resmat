import edu.knuca.resmat.exam.{TestOptionConf, TestOptionValueType}
import edu.knuca.resmat.exam.testset.{TestUtils, VerifiedTestAnswerDto}
import org.scalatest.{FunSpec, Matchers}

class TestUtilsSpec extends FunSpec with Matchers {

  describe("single input") {
    
    it("1") {
      val n1 = "0.123"
      val n2 = "0.123"
      compare(n1, n2, 0.1, result = true)
      compare(n1, n2, 0.01, result = true)
      compare(n1, n2, 0.001, result = true)
      compare(n1, n2, 0.0001, result = true)
      compare(n1, n2, 0.00001, result = true)
    }

    it("2") {
      val n1 = "5.45"
      val n2 = "5.46"
      compare(n1, n2, 0.1, result = true)
      compare(n1, n2, 0.01, result = false)
    }
    
    it("3") {
      val n1 = "0.0000004567d"
      val n2 = "0.0000003567d"

      compare(n1, n2, 0.00001, result = true)
//      compare(n1, n2, 0.000001, result = true)
      compare(n1, n2, 0.0000001, result = false)
    }
    
    it("4") {
      val n1 = "0.1"
      val n2 = "0.123"

      compare(n1, n2, 0.1, result = true)
      compare(n1, n2, 0.01, result = false)
    }
    
  }
  
  def compare(n1: String, n2: String, precision: Double, result: Boolean): Unit = {
    compareOne(n1, n2, precision, result)
    compareOne(n2, n1, precision, result)
  }
  
  def compareOne(n1: String, n2: String, precision: Double, result: Boolean): Unit = {
    val testConfId = 1
    val correctOption = TestOptionConf(-1, n2, valueType = TestOptionValueType.Number)
    val expectedAnswer = VerifiedTestAnswerDto(testConfId, result, if (!result) 1 else 0, Map(correctOption.id -> result))
    TestUtils.verifySingleInputTest(testConfId, n1, correctOption, Some(precision)) shouldEqual expectedAnswer
  }

}
