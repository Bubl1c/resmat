package edu.knuca.resmat.core

import breeze.linalg.DenseVector
import edu.knuca.resmat.core.crosssection.{GeometryShape, PlastynaShape, ShapeRotationAngle, XYCoords}

import scala.math._

//TODO Doesnt match
// CentralMomentsOfInertia I_zc
// MainCoordinateSystem
// MainMomentsOfInertia
// MainMomentsOfInertiaCheck
// RadiusesOfInertia i_v

object CrossSectionSolver extends App {
  val shapes: Vector[GeometryShape] = Vector(
    PlastynaShape(1, "Plastyna1", ShapeRotationAngle.R0, XYCoords(5, 2), List.empty, 2, 6),
    PlastynaShape(2, "Plastyna2", ShapeRotationAngle.R0, XYCoords(3, 2), List.empty, 3, 2)
  )
  val input = CrossSectionProblemInput(shapes.map(_.getShapeInput))

  val answer = new CrossSectionSolver(input).solve()

  println(answer)

}

/**
  * Графіка
  *
  * Допоміжна система координат - центр це права крайня верхня точка фігури
  *
  * Загальна (центральна) система координат - не повернута в центрі ваги
  * Головна система координат - загальна повернута на alphaDegrees
  *
  * Еліпс повертається на alphaDegrees
  * i_u - Радіус еліпса ПЕРПЕНДИКУЛЯРНО осі U
  * i_v - Радіус еліпса ПЕРПЕНДИКУЛЯРНО осі U
  *
  */

/**
  * по пдф документу
  *
  * 1. формування варіанту (врахувати що для різних фігур вводяться різні значення)
  * 2. виконання алгоритму
  *
  * Воркфлоу
  * Варіант
  *  - допоміжна вісь
  *  - фігури
  *   - номер в сортаменті (або розміри) !!!!
  *   - номер по порядку
  * TODO Спробувати малювати додаткові осі і еліпс на кресленні в варіанті, якщо не вийде додати helpStep
  * 2. Визначення геометричних характеристик окремих елементів складеного перерізу
  *   - намальована фігура
  *   - InputSet по кожній з хар-к
  * 2. Визначенняцентрувагискладеногопоперечногоперерізувсистемікоординат y00z0
  *   - Sz0 == r1
  *   - Sy0 == r3
  *   - Sum[Ai] == A_sum
  *   - yc = y_center
  *   - zc = z_center
  *   Замінити на зручніший вигляд InputSet
  *   TODO Домалювати Центральну вісь на кресленні
  * 3. Визначеннявідстаніміжцентральнимиосями - об'єднати з 4
  * 4. SumAi == A_sum - відобразити як статичні значення. ai - input, постаратись відобразити як формулу
  *    Syc, Szc ~~ 0 але всеодно перевірити (поставити знак приблизно)
  *    Підписувати номер фігури в формулі
  * 5. Iyi (момент інерції окремої фігури), Ai (площа окремої фігури), ai - див пункт 4
  *    Підписувати номер фігури в формулі
  * 6. Визначаємо відцентровий момент інерції
  *    Підписувати номер фігури в формулі
  * 7. Перевірити alphaDegrees
  * 8. Визначення головних центральних моментів інерції
  *    I_u - input
  *    I_v - input
  * 9. Перевірка головних центральних моментів інерції
  * 10. Головні радіуси інерції
  * 11. Відобразити фінаьне креслення
  *
  * Додатково
  * Визначати відстані від крайніх точок до осей і перевіряти більшу
  *
  *
  */


//TODO: Precision .0001

class CrossSectionSolver(input: CrossSectionProblemInput) {
  private val n = input.shapes.size
  println(s"Amount of shapes n=$n")

  case class CrossSectionProblemInputVectors(
    area: DenseVector[Double],
    I_y: DenseVector[Double],
    I_z: DenseVector[Double],
    I_yz: DenseVector[Double],
    y_c: DenseVector[Double],
    z_c: DenseVector[Double]
  )

  //задається варіантом попередньо
  private val area = DenseVector[BigDecimal](input.shapes.map(s => BigDecimal(s.square)).toArray)
  private val I_y = input.shapes.map(_.I_y)
  private val I_z = input.shapes.map(_.I_z)
  private val I_yz = input.shapes.map(_.I_yz)
  //кординати фігури
  private val y_c = input.shapes.map(_.y_center)
  private val z_c = input.shapes.map(_.z_center)

  //Координати загального центра ваги
  private var r1: BigDecimal = 0.0
  private var A_sum: BigDecimal = 0.0
  private var r3: BigDecimal = 0.0
  private var y_center: BigDecimal = 0.0
  private var z_center: BigDecimal = 0.0
  for(i <- 0 until n){
    r1 += area(i) * y_c(i)
    A_sum += area(i)
    y_center = (r1 / A_sum).doubleValue()

    r3 += area(i) * z_c(i);
    z_center = (r3 / A_sum).doubleValue()
  }
  println(s"y_center=$y_center")
  println(s"z_center=$z_center")

  // координати центрів ваги складових фігур
  private val a = DenseVector.zeros[BigDecimal](n)
  private val aOutput = scala.collection.mutable.ListBuffer[ShapeDistanceToCentralAxis]()
  private val b = DenseVector.zeros[BigDecimal](n)
  private val bOutput = scala.collection.mutable.ListBuffer[ShapeDistanceToCentralAxis]()
  for (i <- 0 until n) {
    a(i) = z_c(i) - z_center
    aOutput += ShapeDistanceToCentralAxis(input.shapes(i).id, a(i).doubleValue())
    b(i) = y_c(i) - y_center
    bOutput += ShapeDistanceToCentralAxis(input.shapes(i).id, b(i).doubleValue())
  }

  // центральні моменти інерції
  private var I_yc_tmp: BigDecimal = 0.0
  private var I_zc_tmp: BigDecimal = 0.0
  private var I_yzc_tmp: BigDecimal = 0.0
  for (i <- 0 until n) {
    I_yc_tmp += (I_y(i) + area(i) * pow(a(i).doubleValue(), 2)).doubleValue()
    I_zc_tmp += (I_z(i) + area(i) * pow(b(i).doubleValue(), 2)).doubleValue()
    I_yzc_tmp += (I_yz(i) + area(i) * a(i) * b(i))
  }
  private val I_yc: Double = I_yc_tmp.doubleValue()
  private val I_zc: Double = I_zc_tmp.doubleValue()
  private val I_yzc: Double = I_yzc_tmp.doubleValue()
  println(s"I_yc=$I_yc")
  println(s"I_zc=$I_zc")
  println(s"I_yzc=$I_yzc")

  //головна система координат
  private val alfaRad: Double = atan(2d * I_yzc / (I_zc - I_yc)) / 2d
  private val alfaDegrees: Double = Math.toDegrees(alfaRad)
  println(s"alfaRad=$alfaRad")
  println(s"alfaDegrees=$alfaDegrees")

  //головні моменти інерції
  private val I_u: Double = I_yc*cos(alfaRad)*cos(alfaRad)+I_zc*sin(alfaRad)*sin(alfaRad)-I_yzc*sin(2d*alfaRad)
  private val I_v: Double = I_zc*cos(alfaRad)*cos(alfaRad)+I_yc*sin(alfaRad)*sin(alfaRad)+I_yzc*sin(2d*alfaRad)
  println(s"I_u=$I_u")
  println(s"I_v=$I_v")

  //Перевірка головних моментів інерції
  private val I_max: Double = (I_yc+I_zc)/2d+pow((pow((I_yc-I_zc)/2d,2)+pow(I_yzc,2)),0.5)
  private val I_min: Double = (I_yc+I_zc)/2d-pow((pow((I_yc-I_zc)/2d,2)+pow(I_yzc,2)),0.5)
  println(s"I_max=$I_max")
  println(s"I_min=$I_min")

  //радіуси інерції
  private val i_u: Double = pow((I_u/A_sum).doubleValue(), 0.5)
  private val i_v: Double = pow((I_v/A_sum).doubleValue(), 0.5)
  println(s"i_u=$i_u")
  println(s"i_v=$i_v")

  def solve(): CrossSectionProblemAnswer = {

    val answer = CrossSectionProblemAnswer(
      input.shapes,
      CenterOfGravity(y_center.doubleValue(), z_center.doubleValue()),
      DistanceBetweenCentralAxes(aOutput.toVector, bOutput.toVector),
      CentralMomentsOfInertia(I_yc, I_zc, I_yzc),
      MainCoordinateSystem(alfaDegrees),
      MainMomentsOfInertia(I_u, I_v),
      MainMomentsOfInertiaCheck(I_max, I_min),
      RadiusesOfInertia(i_u, i_v)
    )
    println(s"Calculated CrossSectionAnswer $answer for input $input")
    answer
  }
}

case class CrossSectionProblemInput(shapes: Vector[ShapeInput])

case class ShapeInput(
  id: Int,
  shapeName: String,
  square: Double,
  I_y: Double,
  I_z: Double,
  I_yz: Double,
  y_center: Double,
  z_center: Double
) {
  override def toString: String = {
    s"""ShapeInput $id $shapeName
      |square   = $square
      |I_y      = $I_y
      |I_z      = $I_z
      |I_yz     = $I_yz
      |y_center = $y_center
      |z_center = $z_center
      |""".stripMargin
  }
}

/**
  * Координати загального центра ваги
  */
case class CenterOfGravity(y_center: Double, z_center: Double) {
  override def toString: String = s"CenterOfGravity y_center = $y_center, z_center = $z_center"
}
/**
  * центральні моменти інерції
  */
case class CentralMomentsOfInertia(I_yc: Double, I_zc: Double, I_yzc: Double) {
  override def toString: String = s"CentralMomentsOfInertia I_yc = $I_yc, I_zc = $I_zc, I_yzc = $I_yzc"
}

/**
  * Головна система координат
  * @param alfaDegrees - кут повороту основної системи координат. Якщо > 0 - проти годинникової
  */
case class MainCoordinateSystem(alfaDegrees: Double)
/**
  * Головні моменти інерції
  */
case class MainMomentsOfInertia(I_u: Double, I_v: Double) {
  override def toString: String = s"MainMomentsOfInertia I_u = $I_u, I_v = $I_v"
}
/**
  * Перевірка головних моментів інерції
  */
case class MainMomentsOfInertiaCheck(I_max: Double, I_min: Double) {
  override def toString: String = s"MainMomentsOfInertiaCheck I_max = $I_max, I_min = $I_min"
}
/**
  * радіуси інерції
  */
case class RadiusesOfInertia(i_u: Double, i_v: Double) {
  override def toString: String = s"RadiusesOfInertia i_u = $i_u, i_v = $i_v"
}

/**
  * Відстані між центральними осями
  */
case class DistanceBetweenCentralAxes(a: Vector[ShapeDistanceToCentralAxis], b: Vector[ShapeDistanceToCentralAxis]) {
  override def toString: String = s"DistanceBetweenCentralAxes a = $a, b = $b"
}

case class ShapeDistanceToCentralAxis(shapeId: Int, distance: Double)

case class CrossSectionProblemAnswer(
  shapeInputs: Vector[ShapeInput],
  centerOfGravity: CenterOfGravity,
  distanceBetweenCentralAxes: DistanceBetweenCentralAxes,
  centralMomentsOfInertia: CentralMomentsOfInertia,
  mainCoordinateSystem: MainCoordinateSystem,
  mainMomentsOfInertia: MainMomentsOfInertia,
  mainMomentsOfInertiaCheck: MainMomentsOfInertiaCheck,
  radiusesOfInertia: RadiusesOfInertia
) extends ProblemAnswer {

  import edu.knuca.resmat.core.CrossSectionProblemAnswer.{Mapping => M}

  override protected val mapping: Map[String, Any] = {
    val static = Map(
      M.y_center -> centerOfGravity.y_center,
      M.z_center -> centerOfGravity.z_center,

      M.I_yc -> centralMomentsOfInertia.I_yc,
      M.I_zc -> centralMomentsOfInertia.I_zc,
      M.I_yzc -> centralMomentsOfInertia.I_yzc,

      M.alfaDegrees -> mainCoordinateSystem.alfaDegrees,

      M.I_u -> mainMomentsOfInertia.I_u,
      M.I_v -> mainMomentsOfInertia.I_v,

      M.I_max -> mainMomentsOfInertiaCheck.I_max,
      M.I_min -> mainMomentsOfInertiaCheck.I_min,

      M.i_u -> radiusesOfInertia.i_u,
      M.i_v -> radiusesOfInertia.i_v
    )
    val inputsMap: Map[String, Double] = shapeInputs.flatMap(si => {
      Map(
        M.Input.square(si.id) -> si.square,
        M.Input.iy(si.id) -> si.I_y,
        M.Input.iz(si.id) -> si.I_z,
        M.Input.iyz(si.id) -> si.I_yz
      )
    }).toMap
    val aMap: Map[String, Double] = distanceBetweenCentralAxes.a.map(ai => M.a(ai.shapeId) -> ai.distance).toMap
    val bMap: Map[String, Double] = distanceBetweenCentralAxes.b.map(bi => M.b(bi.shapeId) -> bi.distance).toMap

    static ++ inputsMap ++ aMap ++ bMap
  }

  override def toString: String = {
    s"""
      |CrossSectionProblemAnswer
      |---------------------------------------------
      |${shapeInputs.mkString("")}
      |$centerOfGravity
      |$centralMomentsOfInertia
      |$distanceBetweenCentralAxes
      |$mainCoordinateSystem
      |$mainMomentsOfInertia
      |$mainMomentsOfInertiaCheck
      |$radiusesOfInertia
      |---------------------------------------------
      |""".stripMargin
  }
}

object CrossSectionProblemAnswer {

  object Mapping {

    object Input {
      def square(shapeId: Int) = s"square_$shapeId"
      def iy(shapeId: Int) = s"iy_$shapeId"
      def iz(shapeId: Int) = s"iz_$shapeId"
      def iyz(shapeId: Int) = s"iyz_$shapeId"
    }

    def a(shapeId: Int) = s"a_$shapeId"
    def b(shapeId: Int) = s"b_$shapeId"

    //CenterOfGravity
    val y_center = "y_center"
    val z_center = "z_center"
    //CentralMomentsOfInertia - Загальні моменти інерції для всієї системи (відносно не повернутої системи)
    val I_yc = "I_zc"
    val I_zc = "I_zc"
    val I_yzc = "I_yzc"
    //MainCoordinateSystem - Положення головної системи координат U-V - центр в центрі ваги загальному
    val alfaDegrees = "alfaDegrees"
    //MainMomentsOfInertia - Головні моменти інерції (відносно повернутої системи)
    val I_u = "I_u"
    val I_v = "I_v"
    // I_uv == 0 в цій системі координат
    //MainMomentsOfInertiaCheck
    val I_max = "I_max"
    val I_min = "I_min"
    //RadiusesOfInertia
    val i_u = "i_u"
    val i_v = "i_v"
  }

}
