package edu.knuca.resmat.core

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.math._
import edu.knuca.resmat.utils.PimpedEnumeration

case class RingPlateResult(del_t: Double,
                           d_e: Double,
                           r1: Vector[Double],
                           gauss: GaussResult,
                           shiftAndForce: ShiftAndForceResult,
                           extremeStress: ExtremeStressResult)

case class GaussResult(b2: Vector[Double])
case class ShiftAndForceResult(w_1: Vector[Double],
                               fi_1: Vector[Double],
                               mr_1: Vector[Double],
                               mt_1: Vector[Double],
                               qr_1: Vector[Double])
case class ExtremeStressResult(g_r: Vector[Double], g_t: Vector[Double], t_t: Vector[Double])



object Main extends App {
  val conf = RingPlateConf(splittingDotsAmount = 11, height = 0.002, moduleE = 200000000.0, poissonRatio = 0.3, q1 = 0d)
  val a = RingPlateSideConf(BindingType.Swivel, length = 0.1, f = 0d, m = 0d, fi = 0d, w = 0.01)
  val b = RingPlateSideConf(BindingType.Hard, length = 1.1, f = 0d, m = 0d, fi = 0d, w = 0d)

  val result = new RingPlateSolver(conf, a, b).solve()
}

class RingPlateSolver(conf: RingPlateConf, a: RingPlateSideConf, b: RingPlateSideConf) {

  //todo is `height_h == t (м)`
  //todo Where is `q (кН/м2)`?
  //todo Where is `Theta_adm (МПа)`


  // кількість точок розбиття пластини
  private val m = conf.splittingDotsAmount
  private val md:Double = conf.splittingDotsAmount
  // геометричні характеристики та матеріал, вводяться ззовні
  private val height_h = conf.height
  private val modul_E = conf.moduleE
  private val koef_Puas = conf.poissonRatio

  private val f_a: Double = a.f
  private val f_b: Double = b.f
  private val m_a: Double = a.m
  private val m_b: Double = b.m
  private val fi_a: Double = a.fi
  private val fi_b: Double = b.fi
  private val w_a: Double = a.w
  private val w_b: Double = b.w
  private val q1: Double = conf.q1   //todo what is this? q?

  // навантаження на внутрішньому
  //та зовнішньому контурі та вимушені переміщення
  private val del_t: Double = (b.length - a.length) / (md - 1)
  println("del_t = " + del_t)
  private val d_e: Double = pow(height_h, 3) * modul_E / 12d / (1d - pow(koef_Puas, 2))
  println("d_e = " + d_e)

  private val r1Helper = DenseVector.zeros[BigDecimal](m)
  r1Helper(0) = BigDecimal(a.length)
  for(i <- 1 until m) {
    r1Helper(i) = r1Helper(i-1) + BigDecimal(del_t)
  }
  // вектор координат точек
  private val r1: DenseVector[Double] = r1Helper.map(_.toDouble)
  printVector("r1", r1)

  // формування системи рівнянь в залежності від гран умов
  private val g1 = DenseMatrix.zeros[Double](4, 5)
  private val b2 = DenseVector.zeros[Double](4)

  def solve(): RingPlateResult = {
    matchBindingTypePrepareData()
    val gaussResult = gaussCalculateEquations
    val (shifAndForceResult, extremeStressResult) = calculateShiftAndForceAndExtremeStress
    RingPlateResult(del_t, d_e, r1, gaussResult, shifAndForceResult, extremeStressResult)
  }

  def matchBindingTypePrepareData(): Unit = {
    a.n match {
      case BindingType.Free =>
        // перше рівняння системи
        if (w_a == 0d) {
          g1(0, 0) = -4d * d_e / r1(0)
          g1(0, 1) = 0d
          g1(0, 2) = 0d
          g1(0, 3) = 0d
          g1(0, 4) = q1 * r1(0) / 2d + f_a
        } else {
          g1(0, 0) = pow(r1(0), 2) * log(r1(0))
          g1(0, 1) = pow(r1(0), 2)
          g1(0, 2) = log(r1(0))
          g1(0, 3) = 1d
          g1(0, 4) = q1 * pow(r1(0), 4) / 64d / d_e + w_a
        }

        // друге рівняння системи
        if (fi_a == 0d) {
          g1(1, 0) = -d_e * (2d * (1d + koef_Puas) * log(r1(0)) + 3d + koef_Puas)
          g1(1, 1) = -2d * d_e * (1d + koef_Puas)
          g1(1, 2) = -d_e * (koef_Puas - 1d) / pow(r1(0), 2)
          g1(1, 3) = 0d
          g1(1, 4) = q1 * (3d + koef_Puas) * pow(r1(0), 2) / 16d + m_a
        } else {
          g1(1, 0) = 2d * r1(0) * log(r1(0)) + r1(0)
          g1(1, 1) = 2d * r1(0)
          g1(1, 2) = 1d / r1(0)
          g1(1, 3) = 0d
          g1(1, 4) = -q1 * pow(r1(0), 3) / 16d / d_e + fi_a
        }

      case BindingType.Swivel =>
        // перше рівняння системи
        if (fi_a == 0d) {
          g1(0, 0) = -d_e * (2d * (1d + koef_Puas) * log(r1(0)) + 3d + koef_Puas)
          g1(0, 1) = -2d * d_e * (1d + koef_Puas)
          g1(0, 2) = -d_e * (koef_Puas - 1d) / pow(r1(0), 2)
          g1(0, 3) = 0d
          g1(0, 4) = q1 * (3d + koef_Puas) * pow(r1(0), 2) / 16d + m_a
        } else {
          g1(0, 0) = 2d * r1(0) * log(r1(0)) + r1(0)
          g1(0, 1) = 2d * r1(0)
          g1(0, 2) = 1d / r1(0)
          g1(0, 3) = 0d
          g1(0, 4) = -q1 * pow(r1(0), 3) / 16d / d_e + fi_a
        }
        // друге рівняння системи
        g1(1, 0) = pow(r1(0), 2) * log(r1(0))
        g1(1, 1) = pow(r1(0), 2)
        g1(1, 2) = log(r1(0))
        g1(1, 3) = 1d
        g1(1, 4) = -q1 * pow(r1(0), 4) / 64d / d_e + w_a

      case BindingType.Hard =>
        // перше рівняння системи
        g1(0, 0) = pow(r1(0), 2) * log(r1(0))
        g1(0, 1) = pow(r1(0), 2)
        g1(0, 2) = log(r1(0))
        g1(0, 3) = 1d
        g1(0, 4) = -q1 * pow(r1(0), 4) / 64d / d_e + w_a

        // друге рівняння системи
        g1(1, 0) = 2d * r1(0) * log(r1(0)) + r1(0)
        g1(1, 1) = 2d * r1(0)
        g1(1, 2) = 1d / r1(0)
        g1(1, 3) = 0d
        g1(1, 4) = -q1 * pow(r1(0), 3) / 16d / d_e + fi_a

    }

    b.n match {
      case BindingType.Free =>
        // третє рівняння системи
        if (w_b == 0d) {
          g1(2, 0) = -4d * d_e / r1(m - 1)
          g1(2, 1) = 0d
          g1(2, 2) = 0d
          g1(2, 3) = 0d
          g1(2, 4) = q1 * r1(m - 1) / 2d + f_b
        }else {
          g1(2, 0) = pow(r1(m - 1), 2) * log(r1(m - 1))
          g1(2, 1) = pow(r1(m - 1), 2)
          g1(2, 2) = log(r1(m - 1))
          g1(2, 3) = 1d
          g1(2, 4) = q1 * pow(r1(m - 1), 4) / 64d / d_e + w_b
          // четверте рівняння системи
          if (fi_b == 0d) {
            g1(3, 0) = -d_e * (2d * (1d + koef_Puas) * log(r1(m - 1)) + 3d + koef_Puas)
            g1(3, 1) = -2d * d_e * (1d + koef_Puas)
            g1(3, 2) = -d_e * (koef_Puas - 1d) / pow(r1(m - 1), 2)
            g1(3, 3) = 0d
            g1(3, 4) = q1 * (3d + koef_Puas) * pow(r1(m - 1), 2) / 16d + m_b
          }
          else {
            g1(3, 0) = 2d * r1(m - 1) * log(r1(m - 1)) + r1(m - 1)
            g1(3, 1) = 2d * r1(m - 1)
            g1(3, 2) = 1d / r1(m - 1)
            g1(3, 3) = 0d
            g1(3, 4) = -q1 * pow(r1(m - 1), 3) / 16d / d_e + fi_b
          }
        }

      case BindingType.Swivel =>
        // третє рівняння системи
        if (fi_b == 0d) {
          g1(2, 0) = -d_e * (2d * (1d + koef_Puas) * log(r1(m - 1)) + 3d + koef_Puas)
          g1(2, 1) = -2d * d_e * (1d + koef_Puas)
          g1(2, 2) = -d_e * (koef_Puas - 1d) / pow(r1(m - 1), 2)
          g1(2, 3) = 0d
          g1(2, 4) = q1 * (3d + koef_Puas) * pow(r1(m - 1), 2) / 16d + m_b
        }
        else {
          g1(2, 0) = 2d * r1(m - 1) * log(r1(m - 1)) + r1(m - 1)
          g1(2, 1) = 2d * r1(m - 1)
          g1(2, 2) = 1d / r1(m - 1)
          g1(2, 3) = 0d
          g1(2, 4) = -q1 * pow(r1(m - 1), 3) / 16d / d_e + fi_b
        }
        // четверте рівняння системи
        g1(3, 0) = pow(r1(m - 1), 2) * log(r1(m - 1))
        g1(3, 1) = pow(r1(m - 1), 2)
        g1(3, 2) = log(r1(m - 1))
        g1(3, 3) = 1d
        g1(3, 4) = -q1 * pow(r1(m - 1), 4) / 64d / d_e + w_b

      case BindingType.Hard =>
        // третє рівняння системи
        g1(2, 0) = pow(r1(m - 1), 2) * log(r1(m - 1))
        g1(2, 1) = pow(r1(m - 1), 2)
        g1(2, 2) = log(r1(m - 1))
        g1(2, 3) = 1d
        g1(2, 4) = -q1 * pow(r1(m - 1), 4) / 64d / d_e + w_b

        // четверте рівняння системи
        g1(3, 0) = 2d * r1(m - 1) * log(r1(m - 1)) + r1(m - 1)
        g1(3, 1) = 2d * r1(m - 1)
        g1(3, 2) = 1d / r1(m - 1)
        g1(3, 3) = 0d
        g1(3, 4) = -q1 * pow(r1(m - 1), 3) / 16d / d_e + fi_b

    }
  }

  def gaussCalculateEquations: GaussResult = {
    // Розвязання системи рівнянь 4 на 4 методом Гауса
    //-----------------------------------------------------------------------
    var tmp = 0d
    val n = 4

    for(i <- 0 until n) {
      tmp = g1(i, i)
      for(j <- n to i by -1) {
        g1(i, j) /= tmp
      }
      for(j <- i+1 until n){
        tmp = g1(j, i)
        for(k <- n to i by -1) {
          g1(j, k) -= tmp * g1(i, k)
        }
      }
    }
    /*зворотній хід*/
    b2(n-1) = g1(n-1, n)
    for(i <- n-2 to 0 by -1) {
      b2(i) = g1(i, n)
      for(j <- i+1 until n) {
        b2(i) -= g1(i, j) * b2(j)
      }
    }
    printVector("b2", b2)
    GaussResult(b2)
  }

  // b2-вектор х-ів
  // Визначення функцій переміщень та зусиль для кожної точки розбиття
  def calculateShiftAndForceAndExtremeStress: (ShiftAndForceResult, ExtremeStressResult) = {
    val w_1 = DenseVector.zeros[Double](m)
    val fi_1 = DenseVector.zeros[Double](m)
    val mr_1 = DenseVector.zeros[Double](m)
    val mt_1 = DenseVector.zeros[Double](m)
    val qr_1 = DenseVector.zeros[Double](m)

    for(i <- 0 until m) {
      w_1(i) = b2(0) * r1(i) * r1(i) * log(r1(i)) + b2(1) * r1(i) * r1(i) + b2(2) * log(r1(i)) + b2(3) + q1 * pow(r1(i), 4) / d_e / 64d
      fi_1(i) = b2(0) * (2d * r1(i) * log(r1(i)) + r1(i)) + 2d * b2(1) * r1(i) + b2(2) / r1(i) + q1 * pow(r1(i), 3) / d_e / 16d
      mr_1(i) = -d_e * (2d * (1d + koef_Puas) * log(r1(i)) + 3d + koef_Puas) * b2(0) - 2d * d_e * (1d + koef_Puas) * b2(1) - d_e * (koef_Puas - 1d) / pow(r1(i), 2) * b2(2) - q1 * (3d + koef_Puas) * pow(r1(i), 2) / 16d
      mt_1(i) = -d_e * (2d * (1d + koef_Puas) * log(r1(i)) + 3d * koef_Puas + 1d) * b2(0) - 2d * d_e * (1d + koef_Puas) * b2(1) - d_e * (-koef_Puas + 1d) / pow(r1(i), 2) * b2(2) - q1 * (3d * koef_Puas + 1d) * pow(r1(i), 2) / 16d
      qr_1(i) = -4d * d_e / r1(i) * b2(0) - q1 * r1(i) / 2d
    }

    //вивід результатів на екран
    printVector("w_1", w_1)
    printVector("fi_1", fi_1)
    printVector("mr_1", mr_1)
    printVector("mt_1", mt_1)
    printVector("qr_1", qr_1)

    // визначеня екстремальних напружень
    val g_r = DenseVector.zeros[Double](m)
    val g_t = DenseVector.zeros[Double](m)
    val t_t = DenseVector.zeros[Double](m)

    for (i <- 0 until m) {
      g_r(i) = 6d * mr_1(i) / pow(height_h, 2)
      g_t(i) = 6d * mt_1(i) / pow(height_h, 2)
      t_t(i) = -3d / 2d * qr_1(i) / height_h
    }

    printVector("g_r", g_r)
    printVector("g_t", g_t)
    printVector("t_t", t_t)
    (ShiftAndForceResult(w_1, fi_1, mr_1, mt_1, qr_1), ExtremeStressResult(g_r, g_t, t_t))
  }

  def printVector(name: String, v: DenseVector[Double]): Unit = {
    println()
    print(name + " = ")
    v.foreach(elem => print(elem + ", "))
  }

  implicit def denseVectorToScalaVector(input: DenseVector[Double]): Vector[Double] = input.toScalaVector()
}

object BindingType extends PimpedEnumeration {
  type BindingType = Value
  val Free = Value(0, "free")     //вільний край
  val Swivel = Value(1, "swivel") //шарнір
  val Hard = Value(2, "hard")     //жорстке
}

case class RingPlateConf(splittingDotsAmount: Int, height: Double, moduleE: Double, poissonRatio: Double, q1: Double)

case class RingPlateSideConf(n: BindingType.BindingType, length: Double, f: Double, m: Double, fi: Double, w: Double)
