package edu.knuca.resmat.data

import edu.knuca.resmat.core.crosssection.{CustomAxesShape, DvotavrShape, EllipseShape, KutykShape, PlastynaShape, ShapeRotationAngle, ShvellerShape, XYCoords}
import edu.knuca.resmat.core.{CrossSectionProblemInput, CrossSectionSolver}
import edu.knuca.resmat.exam._
import io.circe.generic.auto._
import io.circe.syntax._
import edu.knuca.resmat.http.JsonProtocol._
import io.circe.JsonObject

object CrossSectionData {

  object Problem {

    import edu.knuca.resmat.core.CrossSectionProblemInput.{Mapping => M}

    private val confs = Vector(
      ProblemInputVariableConf(id = 1, name = "shapeIds", units = "string with shape ids delimited by comma", alias = M.shapeIds, showInExam = false),
      ProblemInputVariableConf(id = 2, name = "Тип фігури", alias = M.Shape.kind, showInExam = false),
      ProblemInputVariableConf(id = 3, name = "Назва фігури", alias = M.Shape.name, showInExam = true),
      ProblemInputVariableConf(id = 4, name = "RootX", alias = M.Shape.rootX, showInExam = false),
      ProblemInputVariableConf(id = 5, name = "RootY", alias = M.Shape.rootY, showInExam = false),
      ProblemInputVariableConf(id = 6, name = "Кут повороту", units = "градусів", alias = M.Shape.rotationAngle, showInExam = true),
      ProblemInputVariableConf(id = 7, name = "Розміри", units = "json", alias = M.Shape.dimensions, showInExam = true)
    )

    private val shapes = Vector(
      PlastynaShape(1, "Пластина 1", ShapeRotationAngle.R0, XYCoords(5, 2), 2, 6),
      KutykShape(2, "Кутик 2", ShapeRotationAngle.R0, XYCoords(3, 2), 20, 3),
      DvotavrShape(3, "Двотавр 3", ShapeRotationAngle.R0, XYCoords(3, 2), 10),
      ShvellerShape(4, "Швеллер 4", ShapeRotationAngle.R0, XYCoords(3, 2), 10),
      EllipseShape(5, "Еліпс 5", ShapeRotationAngle.R0, XYCoords(3, 2), 5.0, 2.0),
      CustomAxesShape(6, "Осі 6", ShapeRotationAngle.R90, XYCoords(3, 2), 10.0, 10.0,
        props = JsonObject.fromMap(Map("xAxisName" -> "u".asJson, "yAxisName" -> "z".asJson))
      )
    )

    private val varValues = CrossSectionProblemInput.toVariant(shapes)

    private val input = CrossSectionProblemInput(shapes)

    val conf: ProblemConf = ProblemConf(2, "Геометрія", ProblemType.CrossSection, confs)

    val solved = new CrossSectionSolver(input).solve()
    val variants: Seq[ProblemVariantConf] = Seq(
      ProblemVariantConf(
        2, 2, ResmatImageType.Geogebra, solved.shapes.map(_.toJson()).asJson.noSpaces, varValues, solved
      )
    )

  }

  object TaskFlow {

    import edu.knuca.resmat.core.CrossSectionProblemAnswer.{Mapping => M}

    private val steps = Seq(
      TaskFlowStepConf(-1, -1, 1, "Step 1",
        TaskFlowStepType.DynamicInputSet, DynamicInputSetConf(
          1,
          "Визначення геометричних характеристик окремих елементів складеного перерізу",
          M.shapeIdsDividedByComma,
          M.Input.nameKey,
          M.Input.jsonKey,
          Seq(
            InputSetInput(1, "Площа фігури", "", "см2", M.Input.squareKey, ""),
            InputSetInput(2, "Iy", "", "см4", M.Input.iyKey, ""),
            InputSetInput(3, "Iz", "", "см4", M.Input.izKey, ""),
            InputSetInput(4, "Iyz", "", "см4", M.Input.iyzKey, "")
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, 2, "Кінець", TaskFlowStepType.Finished, "{}")
    )

    val taskFlowConf: TaskFlowConfDto = TaskFlowConfDto(
      TaskFlowConf(-1, 2, "Геометрія"),
      steps
    )
  }


}
