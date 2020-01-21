package edu.knuca.resmat.core.crosssection

import edu.knuca.resmat.core.{ShapeInput, Sortament}
import edu.knuca.resmat.utils.PimpedEnumeration

import scala.math.pow

case class XYCoords(x: Double, y: Double)
case class ShapePoint(name: String, coords: XYCoords)

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
}

sealed trait GeometryShape {
  val id: Int
  val name: String
  val shapeType: ShapeType.ShapeType
  val rotationAngle: ShapeRotationAngle.ShapeRotationAngle
  val root: XYCoords
  val points: List[ShapePoint]

  def getShapeInput: ShapeInput

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
}

case class KutykShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  points: List[ShapePoint],
  sortamentKey: String
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Kutyk
  private lazy val sortamentData = Sortament.sortament.kutyk.getOrElse(
    sortamentKey,
    throw new IllegalArgumentException(s"Kutyk with sortament key $sortamentKey doesn't exist")
  )

  def getShapeInput: ShapeInput = {
    val iyz = if (isNegativeShapeSign) -sortamentData.I_x_y else sortamentData.I_x_y
    val center = XYCoords(root.x - sortamentData.z_0, root.y - sortamentData.z_0)
    ShapeInput(
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
}

case class ShvellerShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  points: List[ShapePoint],
  sortamentKey: String
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Kutyk
  private lazy val sortamentData = Sortament.sortament.shveller.getOrElse(
    sortamentKey,
    throw new IllegalArgumentException(s"Shveller with sortament key $sortamentKey doesn't exist")
  )

  def getShapeInput: ShapeInput = {
    val iy = rotationAngle match {
      case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => sortamentData.I_x
      case _ => sortamentData.I_y
    }
    val iz = rotationAngle match {
      case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => sortamentData.I_y
      case _ => sortamentData.I_x
    }
    val iyz = 0
    val center = XYCoords(root.x - sortamentData.z_0, root.y - sortamentData.h/2)
    ShapeInput(
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
}

case class DvotavrShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  points: List[ShapePoint],
  sortamentKey: String
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Dvotavr
  private lazy val sortamentData = Sortament.sortament.dvotavr.getOrElse(
    sortamentKey,
    throw new IllegalArgumentException(s"Dvotavr with sortament key $sortamentKey doesn't exist")
  )

  def getShapeInput: ShapeInput = {
    val iy = rotationAngle match {
      case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => sortamentData.I_x
      case _ => sortamentData.I_y
    }
    val iz = rotationAngle match {
      case ShapeRotationAngle.R0 | ShapeRotationAngle.R180 => sortamentData.I_y
      case _ => sortamentData.I_x
    }
    val iyz = 0
    val center = XYCoords(root.x, root.y - sortamentData.h/2)
    ShapeInput(
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
}

case class KoloShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  points: List[ShapePoint],
  diametr: Double
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Kolo

  def getShapeInput: ShapeInput = {
    val square = Math.PI * pow(diametr, 2) / 4
    val iy = Math.PI * pow(diametr, 4) / 64
    val iz = iy
    val iyz = 0
    val center = XYCoords(root.x, root.y)
    ShapeInput(
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
}

/**
  * Root is left point
  */
case class NapivkoloShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  points: List[ShapePoint],
  diametr: Double
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Napivkolo

  def getShapeInput: ShapeInput = {
    val square = Math.PI * pow(diametr, 2) / 8
    val iy = 0.00686 * pow(diametr, 4)
    val iz = Math.PI * pow(diametr, 4) / 128
    val iyz = 0
    val center = XYCoords(root.x - diametr/2, root.y - diametr*0.212)
    ShapeInput(
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
}

case class Trykutnyk90Shape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  points: List[ShapePoint],
  b: Double,
  h: Double
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Trykutnyk90

  def getShapeInput: ShapeInput = {
    val square = 1/2 * h * b
    val iy = b * pow(h, 3) / 36
    val iz = pow(b, 3) * h / 36
    val iyz = - pow(b, 2) * pow(b, 2) / 72
    val center = XYCoords(root.x - b/3, root.y - h/3)
    ShapeInput(
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
}

case class TrykutnykRBShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  points: List[ShapePoint],
  b: Double,
  h: Double
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.TrykutnykRB

  def getShapeInput: ShapeInput = {
    val square = 1/2 * h * b
    val iy = b * pow(h, 3) / 36
    val iz = pow(b, 3) * h / 48
    val iyz = 0
    val center = XYCoords(root.x - b/2, root.y - h/3)
    ShapeInput(
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
}

case class PlastynaShape(
  id: Int,
  name: String,
  rotationAngle: ShapeRotationAngle.ShapeRotationAngle,
  root: XYCoords,
  points: List[ShapePoint],
  b: Double,
  h: Double
) extends GeometryShape {
  val shapeType: ShapeType.ShapeType = ShapeType.Plastyna

  def getShapeInput: ShapeInput = {
    val iy = b * pow(h, 3) / 12
    val iz = pow(b, 3) * h / 12
    val iyz = 0
    val center = XYCoords(root.x - b/2, root.y - h/2)
    ShapeInput(
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
}