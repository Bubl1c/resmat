package edu.knuca.resmat.data

import edu.knuca.resmat.core.crosssection.{PlastynaShape, ShapeRotationAngle, XYCoords}
import edu.knuca.resmat.core.{CrossSectionProblemInput, CrossSectionSolver}
import edu.knuca.resmat.exam._
import io.circe.generic.auto._
import io.circe.syntax._
import edu.knuca.resmat.http.JsonProtocol._

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
      PlastynaShape(1, "Plastyna1", ShapeRotationAngle.R0, XYCoords(5, 2), List.empty, 2, 6),
      PlastynaShape(2, "Plastyna2", ShapeRotationAngle.R0, XYCoords(3, 2), List.empty, 3, 2)
    )

    private val varValues = CrossSectionProblemInput.toVariant(shapes)

    private val input = CrossSectionProblemInput(shapes.map(_.getShapeInput))

    val conf: ProblemConf = ProblemConf(2, "Геометрія", ProblemType.CrossSection, confs)

    val variants: Seq[ProblemVariantConf] = Seq(
      ProblemVariantConf(
        2, 2, ProblemVariantSchemaType.Geogebra, "",
        varValues,
        new CrossSectionSolver(input).solve()
      )
    )

  }

  object TaskFlow {

    import edu.knuca.resmat.core.CrossSectionProblemAnswer.{Mapping => M}

    private val steps = Seq(
      TaskFlowStepConf(-1, -1, 1, "",
        TaskFlowStepType.InputSet, InputSet(
          1, "InputSetName", Seq(
            InputSetInput(1, "Скільки фігур?", "", "", M.amountOfShapes)
          )
        ).normalised.asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, 2, "Кінець", TaskFlowStepType.Finished, "{}")
    )

    val taskFlowConf: TaskFlowConfDto = TaskFlowConfDto(
      TaskFlowConf(-1, 2, "Порядок виконання задачі"),
      steps
    )
  }


}
