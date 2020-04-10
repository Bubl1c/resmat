package edu.knuca.resmat.core

import edu.knuca.resmat.core.crosssection.GeometryShape
import edu.knuca.resmat.exam.ProblemInputVariableValue
import io.circe.generic.JsonCodec

//Both needed for ProblemVariantInputData
import io.circe.generic.auto._
import edu.knuca.resmat.http.JsonProtocol._

@JsonCodec sealed trait ProblemVariantInputData

case class InputVariableValuesProblemVariantInputData(
  inputVariableValues: Seq[ProblemInputVariableValue]
) extends ProblemVariantInputData

case class GeometryShapesProblemVariantInputData(
  shapes: Vector[GeometryShape]
) extends ProblemVariantInputData

object ProblemVariantInputData // to make JsonCodec work