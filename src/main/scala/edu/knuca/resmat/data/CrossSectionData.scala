package edu.knuca.resmat.data

import edu.knuca.resmat.core.crosssection.{CustomAxesShape, DvotavrShape, EllipseShape, KutykShape, PlastynaShape, ShapeRotationAngle, ShvellerShape, SizeDirections, XYCoords}
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
      ProblemInputVariableConf(id = 3, name = "Назва фігури", alias = M.Shape.name, showInExam = false),
      ProblemInputVariableConf(id = 4, name = "RootX", alias = M.Shape.rootX, showInExam = false),
      ProblemInputVariableConf(id = 5, name = "RootY", alias = M.Shape.rootY, showInExam = false),
      ProblemInputVariableConf(id = 6, name = "Кут повороту", units = "градусів", alias = M.Shape.rotationAngle, showInExam = false),
      ProblemInputVariableConf(id = 7, name = "Розміри", alias = M.Shape.dimensions, showInExam = true)
    )

    private val shapes = Vector(
      DvotavrShape(1, "Двотавр", ShapeRotationAngle.R0, XYCoords(25, 2), 10),
      ShvellerShape(2, "Швеллер", ShapeRotationAngle.R180, XYCoords(71, 2), 10)
    )

    private val varValues = CrossSectionProblemInput.toVariant(shapes)

    private val input = CrossSectionProblemInput(shapes)

    val conf: ProblemConf = ProblemConf(2, "Геометрія", ProblemType.CrossSection, confs)

    val solved = new CrossSectionSolver(input).solve()
    val variants: Seq[ProblemVariantConf] = Seq(
      ProblemVariantConf(
        2, 2, ResmatImageType.Geogebra, solved.shapes.asJson.noSpaces, varValues, solved
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
          M.Input.titleKey,
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
