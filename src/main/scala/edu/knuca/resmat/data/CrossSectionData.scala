package edu.knuca.resmat.data

import edu.knuca.resmat.core.crosssection.{CustomAxesSettings, CustomAxesShape, DvotavrShape, EllipseShape, GeometryShape, GeometryShapeInGroupSettingsJson, KutykShape, PlastynaShape, ShapeRotationAngle, ShvellerShape, SizeDirections, XYCoords}
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
    
    val conf: ProblemConf = ProblemConf(
      2,
      "Геометрія",
      ProblemType.CrossSection,
      confs,
      ProblemConfProps(Seq("img/sortament.pdf"), Some(CustomAxesSettings("y0", "z0", isInverted = true, root = Some(XYCoords(0, 0)))))
    )
    
    // 1. Поміняти знак в координатах рута
    // 2. Взяти протилежний кут повороту
    val variants: Seq[ProblemVariantConf] = Seq(
//      makeVariant(2, 2, Vector(
//        ShvellerShape(1, "Швеллер №20", ShapeRotationAngle.R90, XYCoords(0, 7.6), 20, rotationPoint = Some(XYCoords(0, 7.6))),
//        KutykShape(2, "Кутик 100х8", ShapeRotationAngle.R0, XYCoords(10, 7.6), 10, 0.8, rotationPoint = Some(XYCoords(10, 7.6)))
//      )),
//      makeVariant(3, 2, Vector(
//        PlastynaShape(1, "Пластина 2x6", ShapeRotationAngle.R180, XYCoords(5, 2), 2, 6, rotationPoint = Some(XYCoords(5, 2))),
//        PlastynaShape(2, "Пластина 3x2", ShapeRotationAngle.R180, XYCoords(3, 2), 3, 2, rotationPoint = Some(XYCoords(3, 2)))
//      )),
//      makeVariant(4, 2, Vector(
//        DvotavrShape(1, "Двотавр №24", ShapeRotationAngle.R180, XYCoords(19, 24), 24, rotationPoint = Some(XYCoords(19, 24))),
//        KutykShape(2, "Кутик 100х8", ShapeRotationAngle.R180, XYCoords(7.5, 24), 7.5, 0.5, rotationPoint = Some(XYCoords(7.5, 24)))
//      )),
      makeVariant(5, 2, Vector(
        KutykShape(1, "Кутик 100х14", ShapeRotationAngle.R0, XYCoords(0, 0), 10, 1.4, rotationPoint = Some(XYCoords(0, 0))),
        ShvellerShape(2, "Швеллер №20", ShapeRotationAngle.R270, XYCoords(30, 10), 30, rotationPoint = Some(XYCoords(30, 8.769)))
      ))
    )
    
    def makeVariant(id: Long, problemConfId: Long, shapes: Vector[GeometryShape]): ProblemVariantConf = {
      val varValues = CrossSectionProblemInput.toVariant(shapes)
      val input = CrossSectionProblemInput(shapes)
      val solved = new CrossSectionSolver(input).solve()
      ProblemVariantConf(
        id, problemConfId, ResmatImageType.Geogebra, solved.shapes.asJson.noSpaces, varValues, solved
      )
    }

  }

  object TaskFlow {

    import edu.knuca.resmat.core.CrossSectionProblemAnswer.{Mapping => M}

    private val steps = Seq(
      TaskFlowStepConf(-1, -1, 1, "Визначення геометричних характеристик окремих елементів складеного перерізу",
        TaskFlowStepType.DynamicInputSet, GroupedInputSetConf(
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
          ),
          groupGraphSettings = Some(GeometryShapeInGroupSettingsJson(Some(CustomAxesSettings("y0", "z0", isInverted = true, root = Some(XYCoords(0, 0)))))),
          groupShapeGraphSettings = Some(GeometryShapeInGroupSettingsJson(Some(CustomAxesSettings("y", "z", isInverted = true))))
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, 2, "Визначення центру ваги складеного поперечного перерізу в системі координат $ y_0 {O_z}_0 $",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "Заповніть коефіцієнти системи рівнянь", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ y_c = \\frac{\\sum_{i=0}^n {S_z}_{0i}}{\\sum_{i=0}^n A_i} = $"), ei(1, M.s_z0), es("/"), ei(2, M.sumOfSquares), es("="), ei(3, M.y_center, "[см] точність 0,001")
              )
            ),
            InputSetEquation(
              2, List[EquationItem](
                es("$ z_c = \\frac{\\sum_{i=0}^n {S_y}_{0i}}{\\sum_{i=0}^n A_i} = $"), ei(4, M.s_y0), es("/"), ei(5, M.sumOfSquares), es("="), ei(6, M.z_center, "[см] точність 0,001")
              )
            ),
            InputSetEquation(
              3, List[EquationItem](
                es("Відкладаємо на рисунку координати $y_c$ та $z_c$ з урахуванням знаків, позначаємо положення центру ваги (точка С) та проводимо центральні осі $y_c, z_c$")
              )
            )
          )
        ).asJson.toString()
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
      TaskFlowStepConf(-1, -1, 4, "На основі формул паралельного переходу визначаємо центральні моменти інерції складеного поперечного перерізу",
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
                es("${}^2)\\;=$"),
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
        ).asJson.toString()
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
        ).asJson.toString()
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
      TaskFlowStepConf(-1, -1, 7, "Визначення головних центральних моментів інерції (формули повороту)",
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
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, 8, "Перевірка головних центральних моментів інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_{max} = \\frac{I_{y_c} + I_{z_c}}{2} + \\sqrt { (\\frac{I_{y_c} - I_{z_c}}{2})^2 + {I_{y_cz_c}}^2 }$"),
                ei(-1, M.I_max, "$[см^4]$ точність 0,001")
              )
            ),
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_{min} = \\frac{I_{y_c} + I_{z_c}}{2} - \\sqrt { (\\frac{I_{y_c} - I_{z_c}}{2})^2 + {I_{y_cz_c}}^2 }$"),
                ei(-1, M.I_min, "$[см^4]$ точність 0,001")
              )
            )
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, 9, "Головні радіуси інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ i_u = \\sqrt \\frac{I_u}{A}$"),
                ei(-1, M.i_u, "$[см]$ точність 0,001")
              )
            ),
            InputSetEquation(
              1, List[EquationItem](
                es("$ i_v = \\sqrt \\frac{I_v}{A}$"),
                ei(-1, M.i_v, "$[см]$ точність 0,001")
              )
            )
          )
        ).asJson.toString(),
        Some(0.1)
      ),
      TaskFlowStepConf(-1, -1, 10, "Кінець", TaskFlowStepType.Finished, "{}")
    )

    val taskFlowConf: TaskFlowConfDto = TaskFlowConfDto(
      TaskFlowConf(-1, 2, "Геометрія"),
      steps
    )
  }


}
