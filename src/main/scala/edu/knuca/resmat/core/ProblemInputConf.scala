package edu.knuca.resmat.core

import edu.knuca.resmat.core.crosssection.{GeometryShape, ShapeCustomAxesSettings}
import edu.knuca.resmat.exam.{ProblemInputVariableConf, ProblemInputVariableValue}
import io.circe.generic.JsonCodec

//Both needed for ProblemInputConf
import io.circe.generic.auto._
import edu.knuca.resmat.http.JsonProtocol._

@JsonCodec sealed trait ProblemInputConf

case class InputVariableValuesProblemInputConf(
  inputVariableConfs: Seq[ProblemInputVariableConf]
) extends ProblemInputConf

case class GeometryShapesProblemInputConf(
  customAxesSettings: Option[ShapeCustomAxesSettings] = None
) extends ProblemInputConf

object ProblemInputConf // to make JsonCodec work