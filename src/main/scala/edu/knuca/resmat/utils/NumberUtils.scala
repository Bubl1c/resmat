package edu.knuca.resmat.utils

object NumberUtils {

  def areAlmostEqual(ethalon: Double, toVerify: Double, precision: Option[Double] = None): Boolean = {
    val scale = BigDecimal(precision.getOrElse(0.01)).scale
    val ethalonBd = BigDecimal(ethalon).setScale(scale, BigDecimal.RoundingMode.HALF_UP)
    val toVerifyBd = BigDecimal(toVerify).setScale(scale, BigDecimal.RoundingMode.HALF_UP)
    ethalonBd.equals(toVerifyBd)
  }

}
