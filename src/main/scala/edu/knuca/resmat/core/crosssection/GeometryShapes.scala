package edu.knuca.resmat.core.crosssection

import edu.knuca.resmat.core.crosssection.ShapeRotationAngle.ShapeRotationAngle
import edu.knuca.resmat.core.{ShapeCalculatedData, Sortament}
import edu.knuca.resmat.exam.ProblemInputVariableValue
import edu.knuca.resmat.utils.PimpedEnumeration

import scala.math.pow
import io.circe.{Json, JsonObject}

/**
  * Always in santimeters
  */
case class XYCoords(x: Double, y: Double)
case class ShapePoint(name: String, coords: XYCoords)

object SizeDirections {
  val UP = "up"
  val DOWN = "down"
  val LEFT = "left"
  val RIGHT = "right"
}

object ShapeRotationAngle extends PimpedEnumeration {
  type ShapeRotationAngle = Value
  val R0: ShapeRotationAngle = Value(0, "0")
  val R90: ShapeRotationAngle = Value(90, "90")
  val R180: ShapeRotationAngle = Value(180, "180")
  val R270: ShapeRotationAngle = Value(270, "270")
}

object ShapeType extends PimpedEnumeration {
  type ShapeType = Value
  val Kutyk: ShapeType = Value(0, "Kutyk")
  val Shveller: ShapeType = Value(1, "Shveller")
  val Dvotavr: ShapeType = Value(2, "Dvotavr")
  val Kolo: ShapeType = Value(3, "Kolo")
  val Napivkolo: ShapeType = Value(4, "Napivkolo")
  val Trykutnyk90: ShapeType = Value(5, "Trykutnyk90")
  val TrykutnykRB: ShapeType = Value(6, "TrykutnykRB")
  val Plastyna: ShapeType = Value(7, "Plastyna")
  val Ellipse: ShapeType = Value(8, "Ellipse")
  val CustomAxes: ShapeType = Value(9, "CustomAxes")
}

case class CustomAxesSettings(xAxisName: String, yAxisName: String, isInverted: Boolean, root: Option[XYCoords] = None)

case class GeometryShapeInGroupSettingsJson(
  customAxesSettings: Option[CustomAxesSettings]
)

case class GeometryShapeInGroupJson(
  shape: GeometryShapeJson,
  settings: GeometryShapeInGroupSettingsJson
)

case class GeometryShapeGroupJson(
  shapes: Seq[GeometryShapeInGroupJson],
  settings: Option[GeometryShapeInGroupSettingsJson]
)

case class GeometryShapeJson (
  id: Int,
  name: String,
  shapeType: ShapeType.ShapeType,
  rotationAngle: Int, //0-360
  root: XYCoords,
  dimensions: Map[String, Double],
  sizeDirections: JsonObject,
  settings: JsonObject,
  props: JsonObject,
  rotationPoint: Option[XYCoords] = None
)

sealed trait GeometryShape {
  val id: Int
  val name: String
  val shapeType: ShapeType.ShapeType
  val rotationAngle: ShapeRotationAngle.ShapeRotationAngle
  val rotationPoint: Option[XYCoords]
  val root: XYCoords
  val sizeDirections: JsonObject
  val settings: JsonObject
  val props: JsonObject

  def getShapeCalculatedData: ShapeCalculatedData

  /**
    * Kutyk, 90Trykutnyk - змінити знак відцентрового моменту I_yz залежно від кута повороту
    *   0, 180 - minus
    *   90, 270 - plus
    * Other - None
    */
  protected def isNegativeShapeSign: Boolean = {
    shapeType match {
      case ShapeType.Kutyk | ShapeType.Trykutnyk90 => rotationAngle match {
        case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => true
        case _ => false
      }
      case _ => false
    }
  }

  /**
    * Get center coords depending on rotation angle
    */
  protected def getRotatedCenterCoords(xAdd: Double, yAdd: Double): XYCoords = {
    rotationAngle match {
      case ShapeRotationAngle.R0 => XYCoords(root.x + xAdd, root.y + yAdd)
      case ShapeRotationAngle.R90 => XYCoords(root.x + yAdd, root.y - xAdd)
      case ShapeRotationAngle.R180 => XYCoords(root.x - xAdd, root.y - yAdd)
      case ShapeRotationAngle.R270 => XYCoords(root.x - yAdd, root.y + xAdd)
      case ra => throw new IllegalArgumentException(s"Unhandled rotation angle $ra")
    }
  }

  /**
    * Get center coords depending on rotation angle
    */
  protected def mmToSm(valueInMm: Double): Double = {
    valueInMm / 10
  }

  protected def smToMm(valueInSm: Double): Double = {
    valueInSm * 10
  }

  def dimensionsToMap(): Map[String, Double]

  def toJson(): GeometryShapeJson = GeometryShapeJson(
    this.id,
    this.name,
    this.shapeType,
    this.rotationAngle.id,
    this.root,
    dimensionsToMap(),
    this.sizeDirections,
    this.settings,
    this.props,
    this.rotationPoint
  )
}

object GeometryShape {
  def fromJson(j: GeometryShapeJson): GeometryShape = {
    j.shapeType match {
      case ShapeType.Kutyk => KutykShape(j)
      case ShapeType.Shveller => ShvellerShape(j)
      case ShapeType.Dvotavr => DvotavrShape(j)
      case ShapeType.Kolo => KoloShape(j)
      case ShapeType.Napivkolo => NapivkoloShape(j)
      case ShapeType.Trykutnyk90 => Trykutnyk90Shape(j)
      case ShapeType.TrykutnykRB => TrykutnykRBShape(j)
      case ShapeType.Plastyna => PlastynaShape(j)
      case ShapeType.Ellipse => EllipseShape(j)
      case ShapeType.CustomAxes => CustomAxesShape(j)
      case _ => throw new IllegalArgumentException(s"Unsupported shape type ${j.shapeType} when creating GeometryShape from GeometryShapeJson: $j")
    }
  }
}

case class KutykShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  b: Double,
  t: Double,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Kutyk

  val bString = s"$b".replace(".0", "")
  val tString = s"${t*10}".replace(".0", "")
  private val sortamentKey = s"${bString}_$tString"
  private lazy val sortamentData = Sortament.sortament.kutyk.getOrElse(
    sortamentKey,
    throw new IllegalArgumentException(s"Kutyk with sortament key $sortamentKey doesn't exist")
  )

  def getShapeCalculatedData: ShapeCalculatedData = {
    val iyz = if (isNegativeShapeSign) -sortamentData.I_x_y else sortamentData.I_x_y
    val center = getRotatedCenterCoords(sortamentData.z_0, sortamentData.z_0)
    ShapeCalculatedData(
      id,
      name,
      sortamentData.square,
      sortamentData.I_x,
      sortamentData.I_x,
      iyz,
      center.x,
      center.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("b" -> this.b, "t" -> this.t)
}
object KutykShape {
  def apply(j: GeometryShapeJson): KutykShape = new KutykShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("b"),
    j.dimensions("t"),
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

case class ShvellerShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  n: Int,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Shveller

  private val sortamentKey = s"$n"
  private lazy val sortamentData = Sortament.sortament.shveller.getOrElse(
    sortamentKey,
    throw new IllegalArgumentException(s"Shveller with sortament key $sortamentKey doesn't exist")
  )

  def getShapeCalculatedData: ShapeCalculatedData = {
    val iy = rotationAngle match {
      case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => sortamentData.I_x
      case _ => sortamentData.I_y
    }
    val iz = rotationAngle match {
      case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => sortamentData.I_y
      case _ => sortamentData.I_x
    }
    val iyz = 0
    val center = getRotatedCenterCoords(sortamentData.z_0, mmToSm(sortamentData.h/2))
    ShapeCalculatedData(
      id,
      name,
      sortamentData.square,
      iy,
      iz,
      iyz,
      center.x,
      center.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("n" -> this.n)
}
object ShvellerShape {
  def apply(j: GeometryShapeJson): ShvellerShape = new ShvellerShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("n").toInt,
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

case class DvotavrShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  n: Int,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Dvotavr

  private val sortamentKey = s"$n"
  private lazy val sortamentData = Sortament.sortament.dvotavr.getOrElse(
    sortamentKey,
    throw new IllegalArgumentException(s"Dvotavr with sortament key $sortamentKey doesn't exist")
  )

  def getShapeCalculatedData: ShapeCalculatedData = {
    val iy = rotationAngle match {
      case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => sortamentData.I_x
      case _ => sortamentData.I_y
    }
    val iz = rotationAngle match {
      case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => sortamentData.I_y
      case _ => sortamentData.I_x
    }
    val iyz = 0
    val center = getRotatedCenterCoords(mmToSm(sortamentData.b/2), mmToSm(sortamentData.h/2))
    ShapeCalculatedData(
      id,
      name,
      sortamentData.square,
      iy,
      iz,
      iyz,
      center.x,
      center.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("n" -> this.n)
}
object DvotavrShape {
  def apply(j: GeometryShapeJson): DvotavrShape = new DvotavrShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("n").toInt,
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

case class KoloShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  diametr: Double,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Kolo

  def getShapeCalculatedData: ShapeCalculatedData = {
    val square = Math.PI * pow(diametr, 2) / 4
    val iy = Math.PI * pow(diametr, 4) / 64
    val iz = iy
    val iyz = 0
    val center = XYCoords(root.x, root.y)
    ShapeCalculatedData(
      id,
      name,
      square,
      iy,
      iz,
      iyz,
      center.x,
      center.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("diametr" -> this.diametr)
}
object KoloShape {
  def apply(j: GeometryShapeJson): KoloShape = new KoloShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("diametr"),
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

/**
  * Root is left point
  */
case class NapivkoloShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  diametr: Double,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Napivkolo

  def getShapeCalculatedData: ShapeCalculatedData = {
    val square = Math.PI * pow(diametr, 2) / 8
    val iy = 0.00686 * pow(diametr, 4)
    val iz = Math.PI * pow(diametr, 4) / 128
    val iyz = 0
    val center = getRotatedCenterCoords(diametr/2, diametr*0.212)
    ShapeCalculatedData(
      id,
      name,
      square,
      iy,
      iz,
      iyz,
      center.x,
      center.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("diametr" -> this.diametr)
}
object NapivkoloShape {
  def apply(j: GeometryShapeJson): NapivkoloShape = new NapivkoloShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("diametr"),
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

case class Trykutnyk90Shape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  b: Double,
  h: Double,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Trykutnyk90

  def getShapeCalculatedData: ShapeCalculatedData = {
    val square = 1/2 * h * b
    val iy = b * pow(h, 3) / 36
    val iz = pow(b, 3) * h / 36
    val iyz = - pow(b, 2) * pow(b, 2) / 72
    val center = getRotatedCenterCoords(b/3, h/3)
    ShapeCalculatedData(
      id,
      name,
      square,
      iy,
      iz,
      iyz,
      center.x,
      center.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("b" -> this.b, "h" -> this.h)
}
object Trykutnyk90Shape {
  def apply(j: GeometryShapeJson): Trykutnyk90Shape = new Trykutnyk90Shape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("b"),
    j.dimensions("h"),
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

case class TrykutnykRBShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  b: Double,
  h: Double,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.TrykutnykRB

  def getShapeCalculatedData: ShapeCalculatedData = {
    val square = 1/2 * h * b
    val iy = b * pow(h, 3) / 36
    val iz = pow(b, 3) * h / 48
    val iyz = 0
    val center = getRotatedCenterCoords(b/2, h/3)
    ShapeCalculatedData(
      id,
      name,
      square,
      iy,
      iz,
      iyz,
      center.x,
      center.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("b" -> this.b, "h" -> this.h)
}
object TrykutnykRBShape {
  def apply(j: GeometryShapeJson): TrykutnykRBShape = new TrykutnykRBShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("b"),
    j.dimensions("h"),
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

case class PlastynaShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  b: Double,
  h: Double,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Plastyna

  def getShapeCalculatedData: ShapeCalculatedData = {
    val iy = b * pow(h, 3) / 12
    val iz = pow(b, 3) * h / 12
    val iyz = 0
    val center = getRotatedCenterCoords(b/2, h/2)
    ShapeCalculatedData(
      id,
      name,
      b * h,
      iy,
      iz,
      iyz,
      center.x,
      center.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("b" -> this.b, "h" -> this.h)
}
object PlastynaShape {
  def apply(j: GeometryShapeJson): PlastynaShape = new PlastynaShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("b"),
    j.dimensions("h"),
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

case class EllipseShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  xR: Double,
  yR: Double,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Ellipse

  def getShapeCalculatedData: ShapeCalculatedData = {
    ShapeCalculatedData(
      id,
      name,
      Math.PI * xR * yR,
      0.0,
      0.0,
      0.0,
      root.x,
      root.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("xR" -> this.xR, "yR" -> this.yR)
}
object EllipseShape {
  def apply(j: GeometryShapeJson): EllipseShape = new EllipseShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("xR"),
    j.dimensions("yR"),
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}

case class CustomAxesShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  xSize: Double,
  ySize: Double,
  sizeDirections: JsonObject = JsonObject.empty,
  settings: JsonObject = JsonObject.empty,
  props: JsonObject = JsonObject.empty,
  rotationPoint: Option[XYCoords] = None
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.CustomAxes

  def getShapeCalculatedData: ShapeCalculatedData = {
    ShapeCalculatedData(
      id,
      name,
      xSize * ySize,
      0.0,
      0.0,
      0.0,
      root.x,
      root.y
    )
  }

  def dimensionsToMap(): Map[String, Double] = Map("xSize" -> this.xSize, "ySize" -> this.ySize)
}
object CustomAxesShape {
  def apply(j: GeometryShapeJson): CustomAxesShape = new CustomAxesShape(
    j.id,
    j.name,
    ShapeRotationAngle(j.rotationAngle),
    j.root,
    j.dimensions("xSize"),
    j.dimensions("ySize"),
    j.sizeDirections,
    j.settings,
    j.props,
    j.rotationPoint
  )
}