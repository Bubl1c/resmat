import edu.knuca.resmat.utils.NumberUtils
import org.scalatest.{FunSpec, Matchers}

class NumberUtilsSpec extends FunSpec with Matchers {

  describe("are almost equal") {
    
    it("1") {
      val n1 = 5.45d
      val n2 = 5.46d

      compare(n1, n2, 0.1, result = true)
      compare(n1, n2, 0.01, result = false)
    }

    it("1.5") {
      val n1 = 5.44d
      val n2 = 5.45d

      compare(n1, n2, 0.1, result = true)
      compare(n1, n2, 0.01, result = false)
    }

    it("2") {
      val n1 = 0.0000003567d
      val n2 = 0.0000004567d

      compare(n1, n2, 0.00001, result = true)
      compare(n1, n2, 0.0000001, result = false)
    }

    it("3") {
      val n1 = 0.1
      val n2 = 0.123

      compare(n1, n2, 0.1, result = true)
      compare(n1, n2, 0.01, result = false)
    }

    it("4") {
      val n1 = 0d
      val n2 = 0.01

      compare(n1, n2, 0.1, result = true)
      compare(n1, n2, 0.01, result = false)
    }
    
  }
  
  def compare(n1: Double, n2: Double, precision: Double, result: Boolean): Unit = {
    NumberUtils.areAlmostEqual(n1, n2, Some(precision)) shouldBe result
    NumberUtils.areAlmostEqual(n2, n1, Some(precision)) shouldBe result

    //negative
    NumberUtils.areAlmostEqual(-n1, -n2, Some(precision)) shouldBe result
    NumberUtils.areAlmostEqual(-n2, -n1, Some(precision)) shouldBe result
  }

}
