package edu.knuca.resmat.core

import java.io.InputStream

import com.typesafe.scalalogging.LazyLogging

import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import edu.knuca.resmat.http.JsonProtocol._

object Sortament extends LazyLogging {
  var privateSortament: SortamentData = _
  private var stream: InputStream = _
  try {
    stream = getClass.getResourceAsStream("/sortament.json")
    val lines: Iterator[String] = scala.io.Source.fromInputStream(stream).getLines
    val json: String = lines.mkString
    privateSortament = decode[SortamentData](json).fold(_=>None,Some(_)).getOrElse(
      throw new RuntimeException(s"Failed to parse sortament data in $json")
    )
  } catch {
    case t: Throwable =>
      logger.error("Failed to load Sortament from resources", t)

  } finally {
    if (stream != null) {
      stream.close()
    }
  }

  val sortament: SortamentData = privateSortament
}

case class SortamentData(
  kutyk: Map[String, KutykSortamentData],
  shveller: Map[String, ShvellerSortamentData],
  dvotavr: Map[String, DvotavrSortamentData]
)

case class KutykSortamentData(
  number: String,
  b: Double,
  t: Double,
  R: Double,
  r: Double,
  square: Double,
  mass: Double,
  I_x: Double,
  W_x: Double,
  i_x: Double,
  I_x_0_I_max: Double,
  i_x_0_i_max: Double,
  I_y_0_I_min: Double,
  W_y_0: Double,
  i_y_0_i_min: Double,
  I_x_1: Double,
  I_x_y: Double,
  z_0: Double
)

case class ShvellerSortamentData(
  number: String,
  mass: Double,
  h: Double,
  b: Double,
  d: Double,
  t: Double,
  square: Double,
  I_x: Double,
  W_x: Double,
  i_x: Double,
  S_x: Double,
  I_y: Double,
  W_y: Double,
  i_y: Double,
  z_0: Double
)

case class DvotavrSortamentData(
  number: String,
  h: Double,
  b: Double,
  s: Double,
  t: Double,
  R: Double,
  r: Double,
  square: Double,
  mass: Double,
  I_x: Double,
  W_x: Double,
  i_x: Double,
  S_x: Double,
  I_y: Double,
  W_y: Double,
  i_y: Double
)
