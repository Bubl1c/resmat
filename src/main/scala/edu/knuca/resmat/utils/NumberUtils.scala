package edu.knuca.resmat.utils

object NumberUtils {

  def areAlmostEqual(n1: Double, n2: Double, precision: Option[Double] = None): Boolean = {
    val scale = BigDecimal(precision.getOrElse(0.01)).scale
    val ethalonBd = BigDecimal(n1).setScale(scale, BigDecimal.RoundingMode.DOWN)
    val toVerifyBd = BigDecimal(n2).setScale(scale, BigDecimal.RoundingMode.DOWN)
    ethalonBd.equals(toVerifyBd)
  }

}


