package edu.knuca.resmat.utils

object NumberUtils {

  def areAlmostEqual(ethalon: Double, toVerify: Double, precision: Option[Double] = None): Boolean = {
    val diff = (ethalon - toVerify).abs
    if(ethalon == 0.0 || ethalon.abs < 0.00000001) {
      toVerify == 0.0
    } else {
      (diff / ethalon).abs <= precision.getOrElse(0.01)
    }
  }

}
