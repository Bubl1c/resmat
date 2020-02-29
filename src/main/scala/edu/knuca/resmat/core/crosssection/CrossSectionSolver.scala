package edu.knuca.resmat.core

import breeze.linalg.DenseVector
import edu.knuca.resmat.core.crosssection.{DvotavrShape, GeometryShape, KoloShape, KutykShape, NapivkoloShape, PlastynaShape, ShapeRotationAngle, ShapeType, ShvellerShape, Trykutnyk90Shape, TrykutnykRBShape, XYCoords}
import edu.knuca.resmat.exam.{ProblemInputVariableConf, ProblemInputVariableValue}

import scala.math._

//TODO Doesnt match
// CentralMomentsOfInertia I_zc
// MainCoordinateSystem
// MainMomentsOfInertia
// MainMomentsOfInertiaCheck
// RadiusesOfInertia i_v

object CrossSectionSolver extends App {
  val shapes: Vector[GeometryShape] = Vector(
    ShvellerShape(1, "Shveller1", ShapeRotationAngle.R270, XYCoords(0, 7.6), 20),
    KutykShape(2, "Kutyk2", ShapeRotationAngle.R180, XYCoords(10, 7.6), 10, 8)
  )
  val input = CrossSectionProblemInput(shapes)

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
  * i_v - Радіус еліпса ПЕРПЕНДИКУЛЯРНО осі V
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
  * 2. Визначення центру ваги складеного поперечного перерізу в системі координат y00 z0
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

  private val shapeInputs = input.shapes.map(_.getShapeCalculatedData)

  //задається варіантом попередньо
  private val area = DenseVector[BigDecimal](shapeInputs.map(s => BigDecimal(s.square)).toArray)
  private val I_y = shapeInputs.map(_.I_y)
  private val I_z = shapeInputs.map(_.I_z)
  private val I_yz = shapeInputs.map(_.I_yz)
  //кординати фігури
  private val y_c = shapeInputs.map(_.y_center)
  private val z_c = shapeInputs.map(_.z_center)

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
    aOutput += ShapeDistanceToCentralAxis(shapeInputs(i).id, a(i).doubleValue())
    b(i) = y_c(i) - y_center
    bOutput += ShapeDistanceToCentralAxis(shapeInputs(i).id, b(i).doubleValue())
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

case class CrossSectionProblemInput(shapes: Vector[GeometryShape])
object CrossSectionProblemInput {

  object Mapping {
    val shapeIds = "shapeIds"

    val shapeIdDelimiter = ","

    def withShapeId(shapeId: String)(key: String): String = s"${shapeId}_$key"

    object Shape {
      val kind = "kind"
      val name = "name"
      val rootX = "rootX"
      val rootY = "rootY"
      val rotationAngle = "rotationAngle"
      val dimensions = "dimensions"
    }
  }

  import CrossSectionProblemInput.{Mapping => M}

  def toVariant(shapes: Vector[GeometryShape]): Vector[ProblemInputVariableValue] = {
    val shapeIds = ProblemInputVariableValue(1, 0, Some(shapes.map(_.id).mkString(M.shapeIdDelimiter)))

    val shapeVals: Vector[ProblemInputVariableValue] = shapes.flatMap(s => {
      val kind = ProblemInputVariableValue(2, 0, Some(s.shapeType.toString), Some(s.id.toString))
      val name = ProblemInputVariableValue(3, 0, Some(s.name), Some(s.id.toString))
      val rootX = ProblemInputVariableValue(4, s.root.x, None, Some(s.id.toString))
      val rootY = ProblemInputVariableValue(5, s.root.y, None, Some(s.id.toString))
      val rotationAngle = ProblemInputVariableValue(6, s.rotationAngle.id, None, Some(s.id.toString))

      val commonVals = Vector(kind, name, rootX, rootY, rotationAngle)

      val specificVals: Vector[ProblemInputVariableValue] = s match {
        case k: KutykShape => Vector(
          ProblemInputVariableValue(7, k.b, None, Some(s"${s.id}.b")),
          ProblemInputVariableValue(7, k.t, None, Some(s"${s.id}.t"))
        )
        case k: ShvellerShape => Vector(ProblemInputVariableValue(7, k.n, None, Some(s"${s.id}.n")))
        case k: DvotavrShape => Vector(ProblemInputVariableValue(7, k.n, None, Some(s"${s.id}.n")))
        case k: KoloShape => Vector(ProblemInputVariableValue(7, k.diametr, None, Some(s"${s.id}.diametr")))
        case k: NapivkoloShape => Vector(ProblemInputVariableValue(7, k.diametr, None, Some(s"${s.id}.diametr")))
        case k: Trykutnyk90Shape => Vector(
          ProblemInputVariableValue(7, k.b, None, Some(s"${s.id}.b")),
          ProblemInputVariableValue(7, k.h, None, Some(s"${s.id}.h"))
        )
        case k: TrykutnykRBShape => Vector(
          ProblemInputVariableValue(7, k.b, None, Some(s"${s.id}.b")),
          ProblemInputVariableValue(7, k.h, None, Some(s"${s.id}.h"))
        )
        case k: PlastynaShape => Vector(
          ProblemInputVariableValue(7, k.b, None, Some(s"${s.id}.b")),
          ProblemInputVariableValue(7, k.h, None, Some(s"${s.id}.h"))
        )
        case k =>throw new IllegalArgumentException(s"Shape $k is unknown, while converting to variant")
      }
      commonVals ++ specificVals
    })
    Vector(shapeIds) ++ shapeVals
  }

  def apply(confs: Seq[ProblemInputVariableConf], vals: Seq[ProblemInputVariableValue]): CrossSectionProblemInput = {
    //key - {ProblemInputVariableConf::alias}
    val valsByConf: Map[String, Seq[ProblemInputVariableValue]] = confs.map(c => c.alias -> vals.filter(v => v.variableConfId == c.id)).toMap

    //key - {ProblemInputVariableConf::alias}
    val m: Map[String, ProblemInputVariableValue] = valsByConf.filter(tuple => tuple._2.size == 1).mapValues(_.head)

    val mm: Map[
      String, //{ProblemInputVariableConf::alias}
      Map[
        String, //{ProblemInputVariableValue::variableKey} -> {shapeId}.{filedName}
        ProblemInputVariableValue
      ]
    ] = valsByConf
      .filter(tuple => tuple._2.size > 1)
      .mapValues(
        _.map(v => v.variableKey.get -> v).toMap
      )

    val shapeIds = m.getOrElse(M.shapeIds, throw new IllegalStateException(s"${M.shapeIds} is not defined in the variant"))
      .strValue
      .getOrElse(throw new IllegalStateException(s"strValue is not defined in the ${M.shapeIds}"))

    val shapes = shapeIds.split(M.shapeIdDelimiter).map(shapeId => {
      def value(key: String): ProblemInputVariableValue = m(M.withShapeId(shapeId)(key))

      val kind = ShapeType.withName(value(M.Shape.kind).strValue.getOrElse(
        throw new IllegalStateException(s"${M.Shape.kind} is not defined in variable values $m")
      ))
      val name = value(M.Shape.name).strValue.getOrElse(
        throw new IllegalStateException(s"${M.Shape.name} is not defined in variable values $m")
      )
      val rootX = value(M.Shape.rootX).value
      val rootY = value(M.Shape.rootY).value
      val rotationAngle = ShapeRotationAngle(value(M.Shape.rotationAngle).value.toInt)

      def sValue(fieldName: String): ProblemInputVariableValue = mm.getOrElse(
        M.Shape.dimensions,
        throw new IllegalStateException(s"${M.Shape.dimensions} is not defined in variable values $m")
      ).getOrElse(
        s"$shapeId.$fieldName",
        throw new IllegalStateException(s"$shapeId.$fieldName is not defined in dimensions of $m")
      )

      def sValueD(fieldName: String): Double = sValue(fieldName).value
      def sValueI(fieldName: String): Int = sValue(fieldName).value.toInt

      kind match {
        case ShapeType.Kutyk => KutykShape(shapeId.toInt, name, rotationAngle, XYCoords(rootX, rootY), sValueD("b"), sValueD("d"))
        case ShapeType.Shveller => ShvellerShape(shapeId.toInt, name, rotationAngle, XYCoords(rootX, rootY), sValueI("n"))
        case ShapeType.Dvotavr => DvotavrShape(shapeId.toInt, name, rotationAngle, XYCoords(rootX, rootY), sValueI("n"))
        case ShapeType.Kolo => KoloShape(shapeId.toInt, name, rotationAngle, XYCoords(rootX, rootY), sValueD("diametr"))
        case ShapeType.Napivkolo => NapivkoloShape(shapeId.toInt, name, rotationAngle, XYCoords(rootX, rootY), sValueD("diametr"))
        case ShapeType.Trykutnyk90 => Trykutnyk90Shape(shapeId.toInt, name, rotationAngle, XYCoords(rootX, rootY), sValueD("b"), sValueD("h"))
        case ShapeType.TrykutnykRB => TrykutnykRBShape(shapeId.toInt, name, rotationAngle, XYCoords(rootX, rootY), sValueD("b"), sValueD("h"))
        case ShapeType.Plastyna => PlastynaShape(shapeId.toInt, name, rotationAngle, XYCoords(rootX, rootY), sValueD("b"), sValueD("h"))
        case _ => throw new IllegalArgumentException(s"Shape kind ${kind} is unknown, while processing var values $m")
      }
    })
    CrossSectionProblemInput(shapes.toVector)
  }
}

case class ShapeCalculatedData(
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
