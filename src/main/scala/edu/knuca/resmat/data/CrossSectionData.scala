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
      TaskFlowStepConf(-1, -1, 1, "Визначення геометричних характеристик окремих елементів складеного перерізу",
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
      TaskFlowStepConf(-1, -1, 2, "Визначення центру ваги складеного поперечного перерізу в системі координат $ y_0 {O_z}_0 $",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "Заповніть коефіцієнти системи рівнянь", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ y_c = \\frac{{S_z}_0}{\\sum_{i=0}^n A_i} = $"), ei(1, M.s_z0), es("/"), ei(2, M.sumOfSquares), es("="), ei(3, M.y_center, "[см] точність 0,001")
              )
            ),
            InputSetEquation(
              2, List[EquationItem](
                es("$ z_c = \\frac{{S_y}_0}{\\sum_{i=0}^n A_i} = $"), ei(4, M.s_y0), es("/"), ei(5, M.sumOfSquares), es("="), ei(6, M.z_center, "[см] точність 0,001")
              )
            ),
            InputSetEquation(
              3, List[EquationItem](
                es("Відкладаємо на рисунку координати $y_c$ та $z_c$ з урахуванням знаків, позначаємо положення центру ваги (точка С) та проводимо центральні осі $y_c, z_c$")
              )
            )
          )
        ).asJson.toString(),
        Some(0.001)
      ),
      TaskFlowStepConf(-1, -1, 3, "Перевірка положення центральної системи координат",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "Заповніть коефіцієнти системи рівнянь", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ a_i $ - відстань $y_c - y_i$")
              )
            ),
            InputSetEquation(
              2, List[EquationItem](
                es("$ b_i $ - відстань $z_c - z_i$")
              )
            ),
            InputSetEquation(
              3, List[EquationItem](
                es("де $i$ це номер фігури")
              )
            ),
            InputSetEquation(
              4, List[EquationItem](
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
                ei(-1, M.S_y_c, "[$ см^3 $] точність 0,01")
              )
            ),
            InputSetEquation(
              5, List[EquationItem](
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
                ei(-1, M.S_z_c, "[$ см^3 $] точність 0,01")
              )
            )
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, 4, "На основі формул паралельного переходу визначаємо центральні моменти інреції складеного поперечного перерізу",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ {I_y}_c = \\sum_{i=0}^n ({I_y}_i + A_i * {a_i}^2) =\\;($"),
                EquationItem(SmartValueForEach(
                  M.shapeIdsDividedByComma,
                  Seq(
                    SmartValueInput(-1, M.Input.iyKey, "Iy"),
                    SmartValueStaticString("+"),
                    SmartValueInput(-1, M.Input.squareKey, "A"),
                    SmartValueStaticString("*"),
                    SmartValueInput(-1, M.aKey, "a")
                  ),
                  Seq(SmartValueStaticString("${}^2)\\;+\\;($"))
                )),
                es("$)\\;=$"),
                ei(-1, M.I_yc, "[$ см^4 $] точність 0,001")
              )
            ),
            InputSetEquation(
              2, List[EquationItem](
                es("$ {I_z}_c = \\sum_{i=0}^n ({I_z}_i + A_i * {b_i}^2) =\\;($"),
                EquationItem(SmartValueForEach(
                  M.shapeIdsDividedByComma,
                  Seq(
                    SmartValueInput(-1, M.Input.izKey, "Iz"),
                    SmartValueStaticString("+"),
                    SmartValueInput(-1, M.Input.squareKey, "A"),
                    SmartValueStaticString("*"),
                    SmartValueInput(-1, M.bKey, "b")
                  ),
                  Seq(SmartValueStaticString("${}^2)\\;+\\;($"))
                )),
                es("${}^2)\\;=$"),
                ei(-1, M.I_zc, "[$ см^4 $] точність 0,001")
              )
            )
          )
        ).asJson.toString(),
        Some(0.001)
      ),
      TaskFlowStepConf(-1, -1, 5, "Визначаємо відцентровий момент інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_{{y_c}{z_c}} = \\sum_{i=0}^n ({I_{yz}}_i + A_i * a_i * b_i) =\\;($"),
                EquationItem(SmartValueForEach(
                  M.shapeIdsDividedByComma,
                  Seq(
                    SmartValueInput(-1, M.Input.iyzKey, "Iyz"),
                    SmartValueStaticString("+"),
                    SmartValueInput(-1, M.Input.squareKey, "A"),
                    SmartValueStaticString("*"),
                    SmartValueInput(-1, M.aKey, "a"),
                    SmartValueStaticString("*"),
                    SmartValueInput(-1, M.bKey, "b")
                  ),
                  Seq(SmartValueStaticString("$)\\;+\\;($"))
                )),
                es("$)\\;=$"),
                ei(-1, M.I_yzc, "[$ см^4 $] точність 0,001")
              )
            )
          )
        ).asJson.toString(),
        Some(0.001)
      ),
      TaskFlowStepConf(-1, -1, 6, "Визначаємо положення головних центральних осей інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ \\alpha = \\frac{1}{2}arctg\\left(\\frac{2*I_{{y_c}{z_c}}}{I_{z_c}-I_{y_c}}\\right) =$"),
                ei(-1, M.alfaDegrees, "$ {}^o $ точність 0,01")
              )
            ),
            InputSetEquation(
              2, List[EquationItem](
                es("Після цього на кресленні потрібно повернути систему координат на кут альфа проти годинникової стрілки додатнє значення")
              )
            )
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, 7, "Визначення головних центральних моментів інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_u = $"),
                ei(-1, M.I_u, "$[см^4]$ точність 0,001")
              )
            ),
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_v = $"),
                ei(-1, M.I_v, "$[см^4]$ точність 0,001")
              )
            )
          )
        ).asJson.toString(),
        Some(0.001)
      ),
      TaskFlowStepConf(-1, -1, 8, "Перевірка головних центральних моментів інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_{max} = $"),
                ei(-1, M.I_max, "$[см^4]$ точність 0,001")
              )
            ),
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_{min} = $"),
                ei(-1, M.I_min, "$[см^4]$ точність 0,001")
              )
            )
          )
        ).asJson.toString(),
        Some(0.001)
      ),
      TaskFlowStepConf(-1, -1, 9, "Головні радіуси інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ i_u = $"),
                ei(-1, M.i_u, "$[см]$ точність 0,001")
              )
            ),
            InputSetEquation(
              1, List[EquationItem](
                es("$ i_v = $"),
                ei(-1, M.i_v, "$[см]$ точність 0,001")
              )
            )
          )
        ).asJson.toString(),
        Some(0.001)
      ),
      TaskFlowStepConf(-1, -1, 10, "Кінець", TaskFlowStepType.Finished, "{}")
    )

    val taskFlowConf: TaskFlowConfDto = TaskFlowConfDto(
      TaskFlowConf(-1, 2, "Геометрія"),
      steps
    )
  }


}
