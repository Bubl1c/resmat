package edu.knuca.resmat.core.ringplate

import breeze.linalg.{DenseMatrix, DenseVector}
import edu.knuca.resmat.core.ProblemAnswer
import edu.knuca.resmat.exam._
import edu.knuca.resmat.utils.PimpedEnumeration

import scala.math._

object RingPlateSolver extends App {
  val conf = RingPlateConf(height = 0.02, moduleE = 200000000.0, poissonRatio = 0.3, q = 1d, sigmaAdm = 160)
  val a = RingPlateSideConf(BindingType.Hard, length = 0.1, f = 0d, m = 0d, fi = 0d, w = 0d)
  val b = RingPlateSideConf(BindingType.Hard, length = 1.1, f = 0d, m = 0d, fi = 0d, w = 0d)

  val result = new RingPlateSolver(RingPlateProblemInput(conf, a, b)).solve()
}

class RingPlateSolver(input: RingPlateProblemInput) {

  def this(variableConfs: Seq[ProblemInputVariableConf], variableValues: Seq[ProblemInputVariableValue]) = {
    this(RingPlateProblemInput(
      variableConfs.map(pc => {
        val varVal = variableValues.find(_.variableConfId == pc.id).get
        (pc, varVal)
      })
    ))
  }

  val conf = input.conf
  val a = input.a
  val b = input.b

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
  private val q1: Double = conf.q

  // навантаження на внутрішньому
  //та зовнішньому контурі та вимушені переміщення
  private val del_t: Double = (b.length - a.length) / (md - 1)
  println("del_t = " + del_t)
  private val d_e: Double = (pow(height_h, 3) * modul_E / 12d / (1d - pow(koef_Puas, 2))) * 1000
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

  def solve(): RingPlateProblemAnswer = {
    matchBindingTypePrepareData()
    val g1s = g1.copy //Save state before g1 is mutated in gaussCalculateEquations
    println()
    printMatrix("g1", g1)
    val gaussResult = gaussCalculateEquations
    val (shifAndForceResult, extremeStressResult) = calculateShiftAndForceAndExtremeStress
    val extremeConditionsResult = calculateExtremeConditions
    println()
    println(extremeConditionsResult)
    val (coordinateResult, isStrengthGuaranteed) = calcCoordinates(extremeStressResult)
    println(coordinateResult)
    println("Is strength guaranteed: " + isStrengthGuaranteed)
    RingPlateProblemAnswer(del_t, d_e, r1, isStrengthGuaranteed,
      gaussResult, shifAndForceResult, extremeStressResult, extremeConditionsResult, coordinateResult, g1s)
  }

  def calcCoordinates(extremeStressResult: ExtremeStressResult): (CoordinateResult, Boolean) = {
    val g_r = extremeStressResult.g_r
    val g_t = extremeStressResult.g_t
    val t_t = extremeStressResult.t_t
    val g_eq = DenseVector.zeros[Double](m)
    for(i <- 0 until m){
      g_eq(i)=pow((pow(g_r(i)-0d, 2) + pow(g_t(i)-0d, 2) + pow(g_r(i)-g_t(i), 2)) * 1d/2d, 0.5)
    }
    var maxi = 0
    var max = g_eq(maxi)
    for(i <- 1 until g_eq.iterableSize) {
      if(g_eq(i) > max) {
        max = g_eq(i)
        maxi = i
      }
    }

    val t_max = t_t.max

    val isStrengthGuaranteed = g_eq(maxi) <= conf.sigmaAdm && t_max <= conf.sigmaAdm/2

    (CoordinateResult(r = r1(maxi), qr = g_r(maxi), qt = g_t(maxi), qeq = g_eq(maxi), tmax = t_t.map(_.abs).max), isStrengthGuaranteed)
  }

  def calculateExtremeConditions: ExtremeConditionsResult = {
    def calcSideFree(sideConf: RingPlateSideConf) = {
      var w: Option[Double] = None
      var fi: Option[Double] = None
      var mr: Option[Double] = None
      var qr: Option[Double] = None
      if(sideConf.w == 0d){
        qr = Some(-sideConf.f)
      } else {
        w = Some(sideConf.w)
      }
      if(sideConf.fi == 0d){
        mr = Some(sideConf.m)
      } else {
        fi = Some(sideConf.fi)
      }
      ExtremeConditionsSideResult(w, fi, mr, qr)
    }
    def calcSideSwivel(sideConf: RingPlateSideConf) = {
      var w: Option[Double] = None
      var fi: Option[Double] = None
      var mr: Option[Double] = None
      var qr: Option[Double] = None
      if(sideConf.fi == 0d){
        mr = Some(sideConf.m)
      } else {
        fi = Some(sideConf.fi)
      }
      w = Some(sideConf.w)
      ExtremeConditionsSideResult(w, fi, mr, qr)
    }
    def calcSideHard(sideConf: RingPlateSideConf) = {
      ExtremeConditionsSideResult(Some(sideConf.w), Some(sideConf.fi), None, None)
    }
    def calcSide(sideConf: RingPlateSideConf) = {
      sideConf.n match {
        case BindingType.Free =>
          calcSideFree(sideConf)
        case BindingType.Swivel =>
          calcSideSwivel(sideConf)
        case BindingType.Hard =>
          calcSideHard(sideConf)
      }
    }
    ExtremeConditionsResult(calcSide(a), calcSide(b))
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
          g1(0, 4) = q1 * r1(0) / 2d - f_a
        } else {
          g1(0, 0) = pow(r1(0), 2) * log(r1(0))
          g1(0, 1) = pow(r1(0), 2)
          g1(0, 2) = log(r1(0))
          g1(0, 3) = 1d
          g1(0, 4) = -q1 * pow(r1(0), 4) / 64d / d_e + w_a
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
        } else {
          g1(2, 0) = pow(r1(m - 1), 2) * log(r1(m - 1))
          g1(2, 1) = pow(r1(m - 1), 2)
          g1(2, 2) = log(r1(m - 1))
          g1(2, 3) = 1d
          g1(2, 4) = -q1 * pow(r1(m - 1), 4) / 64d / d_e + w_b
        }
        // четверте рівняння системи
        if (fi_b == 0d) {
          g1(3, 0) = -d_e * (2d * (1d + koef_Puas) * log(r1(m - 1)) + 3d + koef_Puas)
          g1(3, 1) = -2d * d_e * (1d + koef_Puas)
          g1(3, 2) = -d_e * (koef_Puas - 1d) / pow(r1(m - 1), 2)
          g1(3, 3) = 0d
          g1(3, 4) = q1 * (3d + koef_Puas) * pow(r1(m - 1), 2) / 16d + m_b
        } else {
          g1(3, 0) = 2d * r1(m - 1) * log(r1(m - 1)) + r1(m - 1)
          g1(3, 1) = 2d * r1(m - 1)
          g1(3, 2) = 1d / r1(m - 1)
          g1(3, 3) = 0d
          g1(3, 4) = -q1 * pow(r1(m - 1), 3) / 16d / d_e - fi_b
        }

      case BindingType.Swivel =>
        // третє рівняння системи
        if (fi_b == 0d) {
          g1(2, 0) = -d_e * (2d * (1d + koef_Puas) * log(r1(m - 1)) + 3d + koef_Puas)
          g1(2, 1) = -2d * d_e * (1d + koef_Puas)
          g1(2, 2) = -d_e * (koef_Puas - 1d) / pow(r1(m - 1), 2)
          g1(2, 3) = 0d
          g1(2, 4) = q1 * (3d + koef_Puas) * pow(r1(m - 1), 2) / 16d + m_b
        } else {
          g1(2, 0) = 2d * r1(m - 1) * log(r1(m - 1)) + r1(m - 1)
          g1(2, 1) = 2d * r1(m - 1)
          g1(2, 2) = 1d / r1(m - 1)
          g1(2, 3) = 0d
          g1(2, 4) = -q1 * pow(r1(m - 1), 3) / 16d / d_e - fi_b
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
        g1(3, 4) = -q1 * pow(r1(m - 1), 3) / 16d / d_e - fi_b

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
      g_r(i) = 6d * mr_1(i) / pow(height_h, 2) / 1000
      g_t(i) = 6d * mt_1(i) / pow(height_h, 2) / 1000
      t_t(i) = -3d / 2d * qr_1(i) / height_h / 1000
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

  def printMatrix(name: String, v: DenseMatrix[Double]): Unit = {
    println(name + " = [")
    println(v.toString())
    println("]")
  }

  implicit def denseVectorToScalaVector(input: DenseVector[Double]): Array[Double] = input.toArray
}

object BindingType extends PimpedEnumeration {
  type BindingType = Value
  val Free = Value(0, "free")     //вільний край
  val Swivel = Value(1, "swivel") //шарнір
  val Hard = Value(2, "hard")     //жорстке
}

case class RingPlateConf(height: Double, moduleE: Double, poissonRatio: Double, q: Double, sigmaAdm: Double, splittingDotsAmount: Int = 11)

case class RingPlateSideConf(n: BindingType.BindingType, length: Double, f: Double, m: Double, fi: Double, w: Double)

case class RingPlateProblemInput(conf: RingPlateConf, a: RingPlateSideConf, b: RingPlateSideConf)
object RingPlateProblemInput {
  def apply(vars: Seq[(ProblemInputVariableConf, ProblemInputVariableValue)]) = {
    val m = vars.map{ case (c, v) => c.alias -> v.value }.toMap
    val ringPlateConf = RingPlateConf(m("height"), m("moduleE"), m("poissonRatio"), m("q"), m("sigmaAdm"))
    val aSide = RingPlateSideConf(BindingType(m("a.n").toInt), m("a.length"), m("a.f"), m("a.m"), m("a.fi"), m("a.w"))
    val bSide = RingPlateSideConf(BindingType(m("b.n").toInt), m("b.length"), m("b.f"), m("b.m"), m("b.fi"), m("b.w"))
    new RingPlateProblemInput(ringPlateConf, aSide, bSide)
  }
}

case class RingPlateProblemAnswer(del_t: Double,
                                  d_e: Double,
                                  r1: Array[Double],
                                  isStrengthGuaranteed: Boolean,
                                  gauss: GaussResult,
                                  shiftAndForce: ShiftAndForceResult,
                                  extremeStress: ExtremeStressResult,
                                  extremeConditions: ExtremeConditionsResult,
                                  coordinateResult: CoordinateResult,
                                  g1: DenseMatrix[Double]) extends ProblemAnswer {

  import RingPlateProblemAnswer.{Mapping => M}

  override protected val mapping: Map[String, Any] = Map(
    M.w_a -> extremeConditions.a.w,
    M.fi_a -> extremeConditions.a.fi,
    M.mr_a -> extremeConditions.a.mr,
    M.qr_a -> extremeConditions.a.qr,
    M.w_b -> extremeConditions.b.w,
    M.fi_b -> extremeConditions.b.fi,
    M.mr_b -> extremeConditions.b.mr,
    M.qr_b -> extremeConditions.b.qr,

    M.x1 -> Some(gauss.b2(0)),
    M.x2 -> Some(gauss.b2(1)),
    M.x3 -> Some(gauss.b2(2)),
    M.x4 -> Some(gauss.b2(3)),

    M.charts -> ChartSet("Епюри", Seq(
      ChartData("W Прогин (1/1000 м)",
        r1,
        shiftAndForce.w_1,
        true
      ),
      ChartData("{phi} Кут повороту (1/1000 рад)",
        r1,
        shiftAndForce.fi_1
      ),
      ChartData("Mr Радіальний момент (кН)",
        r1,
        shiftAndForce.mr_1,
        true
      ),
      ChartData("M{theta} Коловий момент (кН)",
        r1,
        shiftAndForce.mt_1,
        true
      ),
      ChartData("Qr Поперечна сила (кН/м)",
        r1,
        shiftAndForce.qr_1
      )
    )),

    M.chartsAsTable -> DynamicTable("Епюри в табличному вигляді", List(), List(
        DynamicTableRow("W", shiftAndForce.w_1.map(d => SmartValueStaticDouble(d)).toList),
        DynamicTableRow("{phi}", shiftAndForce.fi_1.map(d => SmartValueStaticDouble(d)).toList),
        DynamicTableRow("Mr", shiftAndForce.mr_1.map(d => SmartValueStaticDouble(d)).toList),
        DynamicTableRow("M{theta}", shiftAndForce.mt_1.map(d => SmartValueStaticDouble(d)).toList),
        DynamicTableRow("Qr", shiftAndForce.qr_1.map(d => SmartValueStaticDouble(d)).toList)
      )
    ),

    M.r -> Some(coordinateResult.r),
    M.sigma_r -> Some(coordinateResult.qr),
    M.sigma_theta -> Some(coordinateResult.qt),
    M.sigma_eq -> Some(coordinateResult.qeq),
    M.tau_max -> Some(coordinateResult.tmax),

    M.g1_00 -> g1(0, 0),
    M.g1_01 -> g1(0, 1),
    M.g1_02 -> g1(0, 2),
    M.g1_03 -> g1(0, 3),
    M.g1_04 -> g1(0, 4),
    M.g1_10 -> g1(1, 0),
    M.g1_11 -> g1(1, 1),
    M.g1_12 -> g1(1, 2),
    M.g1_13 -> g1(1, 3),
    M.g1_14 -> g1(1, 4),
    M.g1_20 -> g1(2, 0),
    M.g1_21 -> g1(2, 1),
    M.g1_22 -> g1(2, 2),
    M.g1_23 -> g1(2, 3),
    M.g1_24 -> g1(2, 4),
    M.g1_30 -> g1(3, 0),
    M.g1_31 -> g1(3, 1),
    M.g1_32 -> g1(3, 2),
    M.g1_33 -> g1(3, 3),
    M.g1_34 -> g1(3, 4),

    M.w1_2 -> Some(shiftAndForce.w_1(2)),
    M.fi1_2 -> Some(shiftAndForce.fi_1(2)),
    M.mr1_2 -> Some(shiftAndForce.mr_1(2)),
    M.qr1_2 -> Some(shiftAndForce.qr_1(2)),

    M.d_e -> Some(d_e),

    M.isStrengthGuranteed -> (if(isStrengthGuaranteed) 1 else 0).toString
  )
}
object RingPlateProblemAnswer {
  object Mapping {
    val plateType = "plateType"

    val w_a = "w_a"
    val fi_a = "fi_a"
    val mr_a = "mr_a"
    val qr_a = "qr_a"
    val w_b = "w_b"
    val fi_b = "fi_b"
    val mr_b = "mr_b"
    val qr_b = "qr_b"

    val x1 = "x1"
    val x2 = "x2"
    val x3 = "x3"
    val x4 = "x4"

    val charts = "charts"
    val chartsAsTable = "chartsAsTable"

    val r = "r"
    val sigma_r = "sigma_r"
    val sigma_theta = "sigma_theta"
    val sigma_eq = "sigma_eq"
    val tau_max = "tau_max"

    val g1_00 = "g1_00"
    val g1_01 = "g1_01"
    val g1_02 = "g1_02"
    val g1_03 = "g1_03"
    val g1_04 = "g1_04"
    val g1_10 = "g1_10"
    val g1_11 = "g1_11"
    val g1_12 = "g1_12"
    val g1_13 = "g1_13"
    val g1_14 = "g1_14"
    val g1_20 = "g1_20"
    val g1_21 = "g1_21"
    val g1_22 = "g1_22"
    val g1_23 = "g1_23"
    val g1_24 = "g1_24"
    val g1_30 = "g1_30"
    val g1_31 = "g1_31"
    val g1_32 = "g1_32"
    val g1_33 = "g1_33"
    val g1_34 = "g1_34"

    val w1_2 = "w1_2"
    val fi1_2 = "fi1_2"
    val mr1_2 = "mr1_2"
    val qr1_2 = "qr1_2"

    val d_e = "d_e"

    val isStrengthGuranteed = "isStrengthGuranteed"
  }
}

case class GaussResult(b2: Array[Double])
case class ShiftAndForceResult(w_1: Array[Double],
                               fi_1: Array[Double],
                               mr_1: Array[Double],
                               mt_1: Array[Double],
                               qr_1: Array[Double])
case class ExtremeStressResult(g_r: Array[Double], g_t: Array[Double], t_t: Array[Double])

case class ExtremeConditionsResult(a: ExtremeConditionsSideResult, b: ExtremeConditionsSideResult) {
  override def toString: String = s"ExtremeConditions: {\n  a: $a \n  b: $b \n}"
}
case class ExtremeConditionsSideResult(w: Option[Double], fi: Option[Double], mr: Option[Double], qr: Option[Double]) {
  override def toString: String = s"w = $w, fi = $fi, mr = $mr, qr = $qr"
}

case class CoordinateResult(r: Double, qr: Double, qt: Double, qeq: Double, tmax: Double) {
  override def toString: String = s"CoordinateResult: {\n r = $r, qr = $qr, qt = $qt, qeq = $qeq, tmax = $tmax\n}"
}
