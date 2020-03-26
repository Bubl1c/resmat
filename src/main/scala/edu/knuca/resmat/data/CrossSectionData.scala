package edu.knuca.resmat.data

import edu.knuca.resmat.core.crosssection.{CustomAxesShape, DvotavrShape, EllipseShape, KutykShape, PlastynaShape, ShapeRotationAngle, ShvellerShape, SizeDirections, XYCoords}
import edu.knuca.resmat.core.{CrossSectionProblemInput, CrossSectionSolver}
import edu.knuca.resmat.data.Utils.{ed, ei, es}
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
//      TaskFlowStepConf(-1, -1, 1, "Визначення геометричних характеристик окремих елементів складеного перерізу",
//        TaskFlowStepType.DynamicInputSet, DynamicInputSetConf(
//          1,
//          "Визначення геометричних характеристик окремих елементів складеного перерізу",
//          M.shapeIdsDividedByComma,
//          M.Input.titleKey,
//          M.Input.jsonKey,
//          Seq(
//            InputSetInput(1, "Площа фігури", "", "см2", M.Input.squareKey, ""),
//            InputSetInput(2, "Iy", "", "см4", M.Input.iyKey, ""),
//            InputSetInput(3, "Iz", "", "см4", M.Input.izKey, ""),
//            InputSetInput(4, "Iyz", "", "см4", M.Input.iyzKey, "")
//          )
//        ).asJson.toString()
//      ),
//      TaskFlowStepConf(-1, -1, 1, "Визначення центру ваги складеного поперечного перерізу в системі координат",
//        TaskFlowStepType.EquationSet, InputSetEquationSystem(
//          "Заповніть коефіцієнти системи рівнянь", List(
//            InputSetEquation(
//              1, List[EquationItem](
//                es("$ y_c = \\frac{{S_z}_0}{\\sum_{i=0}^n A_i} = $"), ei(1, M.s_z0), es("/"), ei(2, M.sumOfSquares), es("="), ei(3, M.y_center, "[см]")
//              )
//            ),
//            InputSetEquation(
//              2, List[EquationItem](
//                es("$ z_c = \\frac{{S_y}_0}{\\sum_{i=0}^n A_i} = $"), ei(4, M.s_y0), es("/"), ei(5, M.sumOfSquares), es("="), ei(6, M.z_center, "[см]")
//              )
//            )
//          )
//        ).asJson.toString()
//      ),
      TaskFlowStepConf(-1, -1, 1, "Визначення центру ваги складеного поперечного перерізу в системі координат",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "Заповніть коефіцієнти системи рівнянь", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ {S_y}_c = \\sum_{i=0}^n A_i * a_i = $"),
                EquationItem(SmartValueForEach(
                  M.shapeIdsDividedByComma,
                  Seq(
                    SmartValueInput(-1, M.Input.squareKey, "A"),
                    SmartValueStaticString("*"),
                    SmartValueInput(-1, M.aKey, "a")
                  ),
                  Seq(SmartValueStaticString("+"))
                )),
                es("="),
                ei(-1, M.S_y_c, "[$ см^3 $]")
              )
            ),
            InputSetEquation(
              1, List[EquationItem](
                es("$ {S_z}_c = \\sum_{i=0}^n A_i * b_i = $"),
                EquationItem(SmartValueForEach(
                  M.shapeIdsDividedByComma,
                  Seq(
                    SmartValueInput(-1, M.Input.squareKey, "A"),
                    SmartValueStaticString("*"),
                    SmartValueInput(-1, M.bKey, "b")
                  ),
                  Seq(SmartValueStaticString("+"))
                )),
                es("="),
                ei(-1, M.S_z_c, "[$ см^3 $]")
              )
            )
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, 3, "Кінець", TaskFlowStepType.Finished, "{}")
    )

    val taskFlowConf: TaskFlowConfDto = TaskFlowConfDto(
      TaskFlowConf(-1, 2, "Геометрія"),
      steps
    )
  }


}
