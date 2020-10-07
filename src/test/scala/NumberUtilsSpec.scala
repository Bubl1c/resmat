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

  describe("are almost equal v2") {

    it("a = 5.45 b = 5.46") { //above 0.5
      val a = 5.45d
      val b = 5.46d

      compareV2(a, b, 0.1, result = true)
      compareV2(a, b, 0.01, result = true)
      compareV2(a, b, 0.03, result = true)
      compareV2(a, b, 0.001, result = false)
    }

    it("a = 5.44 b = 5.45") { //below 0.5
      val a = 5.44d
      val b = 5.45d

      compareV2(a, b, 0.1, result = true)
      compareV2(a, b, 0.01, result = true)
      compareV2(a, b, 0.03, result = true)
      compareV2(a, b, 0.001, result = false)
    }

    it("a = 5.444 b = 5.445") { //below 0.5
      val a = 5.444d
      val b = 5.445d

      compareV2(a, b, 0.1, result = true)
      compareV2(a, b, 0.01, result = true)
      compareV2(a, b, 0.03, result = true)
      compareV2(a, b, 0.001, result = true)
      compareV2(a, b, 0.0001, result = false)
    }

    it("a = 0.1 b = 0.123") {
      val a = 0.1
      val b = 0.123

      compareV2(a, b, 0.3, result = true)
      compareV2(a, b, 0.03, result = false)
      compareV2(a, b, 0.01, result = false)
    }

    it("a = 0.0000003567 b = 0.0000004567") {
      val a = 0.0000003567d
      val b = 0.000000347d

      compareV2(a, b, 0.03, result = true)
    }

    it("a = 0 b = 0.001") {
      val a = 0d
      val b = 0.001
      
      compareV2(a, b, 0.03, result = true, reverse = false)
    }

    it("a = 0 b = 0.01") {
      val a = 0d
      val b = 0.01

      compareV2(a, b, 0.03, result = false, reverse = false)
    }

  }
  
  def compare(n1: Double, n2: Double, precision: Double, result: Boolean): Unit = {
    NumberUtils.areAlmostEqual(n1, n2, Some(precision)) shouldBe result
    NumberUtils.areAlmostEqual(n2, n1, Some(precision)) shouldBe result

    //negative
    NumberUtils.areAlmostEqual(-n1, -n2, Some(precision)) shouldBe result
    NumberUtils.areAlmostEqual(-n2, -n1, Some(precision)) shouldBe result
  }

  def compareV2(a: Double, b: Double, c: Double, result: Boolean, reverse: Boolean = true): Unit = {
    assert(NumberUtils.areAlmostEqualV2(a, b, Some(c)) == result, s"a=$a b=$b c=$c result=$result")
    assert(NumberUtils.areAlmostEqualV2(-a, -b, Some(c)) == result, s"negative a=$a b=$b c=$c result=$result")
    
    if (reverse) {
      assert(NumberUtils.areAlmostEqualV2(b, a, Some(c)) == result, s"a=$b b=$a c=$c result=$result")
      assert(NumberUtils.areAlmostEqualV2(-b, -a, Some(c)) == result, s"negative a=$b b=$a c=$c result=$result")
    }
  }

}
