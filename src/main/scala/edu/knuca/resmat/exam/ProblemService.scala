package edu.knuca.resmat.exam

import com.typesafe.scalalogging.LazyLogging
import edu.knuca.resmat.core.{RingPlateProblemInput, RingPlateSolver}
import edu.knuca.resmat.db.DatabaseService
import edu.knuca.resmat.exam.{ProblemInputVariableConf => VarConf}
import edu.knuca.resmat.exam.{ProblemInputVariableValue => VarVal}

import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext

class ProblemService(val db: DatabaseService)(implicit val executionContext: ExecutionContext) extends LazyLogging {

  import edu.knuca.resmat.http.JsonProtocol._

  val problemConfs: List[ProblemConf] = List(
    ProblemConf(1, "Кільцева пластина", ProblemType.RingPlate, Seq(
      VarConf(2, "Fa", "кН/м", "a.f"),
      VarConf(3, "Ma", "кНм/м", "a.m"),
      VarConf(4, "wa", "м", "a.w"),
      VarConf(5, "{phi}{a}", "рад", "a.fi"),

      VarConf(6, "E", "МПа", "moduleE"),
      VarConf(7, "{mu}", "", "poissonRatio"),
      VarConf(8, "q", "кН/м^2", "q"),

      VarConf(9, "Fb", "кН/м", "b.f"),
      VarConf(10, "Mb", "кНм/м", "b.m"),
      VarConf(11, "wb", "м", "b.w"),
      VarConf(12, "{phi}{b}", "рад", "b.fi"),

      VarConf(13, "a", "м", "a.length"),
      VarConf(14, "b", "м", "b.length"),
      VarConf(15, "t", "м", "height"),

      VarConf(16, "an", "", "a.n", false),
      VarConf(17, "bn", "", "b.n", false),
      VarConf(18, "sigmaAdm", "", "sigmaAdm", false)
    ).asJson.toString())
  )

  val varVals: List[ProblemInputVariableValue] = List(
    VarVal(2, 0),
    VarVal(3, 0),
    VarVal(4, -0.01),
    VarVal(5, 0),

    VarVal(6, 200000000.0),
    VarVal(7, 0.3),
    VarVal(8, 0d),

    VarVal(9, 0),
    VarVal(10, 0),
    VarVal(11, 0),
    VarVal(12, 0),

    VarVal(13, 0.1),
    VarVal(14, 1.1),
    VarVal(15, 0.02),

    VarVal(16, 1),
    VarVal(17, 2),
    VarVal(18, 160)
  )

  val decodedConfs = decode[Seq[VarConf]](problemConfs.find(_.id == 1).get.inputVariableConfs).fold(_ => None, Some(_)).getOrElse(
    throw new RuntimeException("Shit is happening")
  )
  val variant1Input = RingPlateProblemInput(decodedConfs.map(pc => {
    val varVal = varVals.find(_.variableConfId == pc.id).get
    (pc, varVal)
  }))
  val problemVariantConfs: List[ProblemVariantConf] = List(
    ProblemVariantConf(1, 1, "img/tasks/9.png",
      varVals.asJson.toString(),
      new RingPlateSolver(variant1Input).solve().asJson.toString()
    )
  )

  def getProblemConfById(id: Long) = problemConfs.find(_.id == id).getOrElse(
    throw new RuntimeException(s"Problem conf with id: $id not found!")
  )

  def getProblemVariantConfById(id: Long): ProblemVariantConf = problemVariantConfs.find(_.id == id).getOrElse(
    throw new RuntimeException(s"Problem variant conf with id: $id not found!")
  )

}
