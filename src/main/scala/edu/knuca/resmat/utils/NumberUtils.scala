package edu.knuca.resmat.utils

import com.typesafe.scalalogging.LazyLogging

object NumberUtils extends LazyLogging {

  def areAlmostEqual(n1: Double, n2: Double, precision: Option[Double] = None): Boolean = {
    val scale = BigDecimal(precision.getOrElse(0.01)).scale
    val ethalonBd = BigDecimal(n1).setScale(scale, BigDecimal.RoundingMode.DOWN)
    val toVerifyBd = BigDecimal(n2).setScale(scale, BigDecimal.RoundingMode.DOWN)
    ethalonBd.equals(toVerifyBd)
  }

  /**
    * @param a правильна відповідь, точність 0.000001
    * @param b відповідь студента
    * @param cOpt точність, 0.03 якщо не вказано
    * @return
    */
  def areAlmostEqualV2(a: Double, b: Double, cOpt: Option[Double] = None): Boolean = {
    val c = cOpt.getOrElse(0.03)
    if (a == 0 || a.abs <= 0.0001) {
      logger.trace(s"a=$a b=$b c=$c b.abs (${b.abs}) <= ${0.001}")
      if (b.abs <= 0.001) {
        true
      } else {
        false
      }
    } else if (a > 0) {
      logger.trace(s"a=$a b=$b c=$c {a > 0} (1 - c) * a (${(1 - c) * a}) <= ($b) <= (${(1 + c) * a}) (1 + c) * a")
      if ((1 - c) * a <= b && b <= (1 + c) * a) {
        true
      } else {
        false
      }
    } else { //a < 0
      logger.trace(s"a=$a b=$b c=$c {a > 0} (1 + c) * a (${(1 + c) * a}) <= ($b) <= (${(1 - c) * a}) (1 - c) * a")
      if ((1 + c) * a <= b && b <= (1 - c) * a) {
        true
      } else {
        false
      }
    }
  }

}


