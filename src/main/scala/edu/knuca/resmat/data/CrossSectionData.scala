package edu.knuca.resmat.data

import edu.knuca.resmat.core.crosssection.{CustomAxesShape, DvotavrShape, EllipseShape, GeometryShape, GeometryShapeInGroupSettingsJson, KutykShape, PlastynaShape, ShapeCustomAxesSettings, ShvellerShape, SizeDirections, XYCoords}
import edu.knuca.resmat.core.{CrossSectionSolver, GeometryShapesProblemInputConf, GeometryShapesProblemVariantInputData}
import edu.knuca.resmat.data.Utils.{ed, ei, es, esd}
import edu.knuca.resmat.exam._
import io.circe.generic.auto._
import io.circe.syntax._
import edu.knuca.resmat.http.JsonProtocol._
import io.circe.JsonObject

object CrossSectionData {

  object Problem {
    
    val conf: ProblemConf = ProblemConf(
      2,
      "Геометрія",
      ProblemType.CrossSection,
      GeometryShapesProblemInputConf(
        Some(ShapeCustomAxesSettings("y0", "z0", root = Some(XYCoords(0, 0))))
      ),
      ProblemConfProps(Seq("img/sortament.pdf"))
    )
    
    // 1. Поміняти знак в координатах рута
    // 2. Взяти протилежний кут повороту
    val variants: Seq[ProblemVariantConf] = Seq(
      makeVariant(2, 2, Vector(
        ShvellerShape(1, "Швеллер №20", 90, XYCoords(0, 7.6), 20, rotationPoint = Some(XYCoords(0, 7.6))),
        KutykShape(2, "Кутик 100х8", 0, XYCoords(10, 7.6), 10, 0.8, rotationPoint = Some(XYCoords(10, 7.6)))
      )),
      makeVariant(3, 2, Vector(
        PlastynaShape(1, "Пластина 2x6", 180, XYCoords(5, 2), 2, 6, rotationPoint = Some(XYCoords(5, 2))),
        PlastynaShape(2, "Пластина 3x2", 180, XYCoords(3, 2), 3, 2, rotationPoint = Some(XYCoords(3, 2)))
      )),
      makeVariant(4, 2, Vector(
        DvotavrShape(1, "Двотавр №24", 180, XYCoords(19, 24), 24, rotationPoint = Some(XYCoords(19, 24))),
        KutykShape(2, "Кутик 75х5", 180, XYCoords(7.5, 24), 7.5, 0.5, rotationPoint = Some(XYCoords(7.5, 24)))
      )),
      makeVariant(5, 2, Vector(
        KutykShape(1, "Кутик 100х14", 0, XYCoords(0, 0), 10, 1.4, rotationPoint = Some(XYCoords(0, 0))),
        ShvellerShape(2, "Швеллер №30", 270, XYCoords(30, 10), 30, rotationPoint = Some(XYCoords(30, 10)))
      )),
      makeVariant(6, 2, Vector(
        DvotavrShape(1, "Двотавр №14", 270, XYCoords(15, 26.7), 14, rotationPoint = Some(XYCoords(15, 26.7))),
        PlastynaShape(2, "Пластина 10x340", 180, XYCoords(1, 34), 1, 34, rotationPoint = Some(XYCoords(1, 34)))
      ))
    )
    
    def makeVariant(id: Long, problemConfId: Long, shapes: Vector[GeometryShape]): ProblemVariantConf = {
      val input = GeometryShapesProblemVariantInputData(shapes)
      val solved = new CrossSectionSolver(input).solve()
      ProblemVariantConf(
        id,
        problemConfId,
        ResmatImageType.Geogebra,
        solved.shapes.asJson.noSpaces,
        GeometryShapesProblemVariantInputData(shapes),
        solved
      )
    }

  }

  object TaskFlow {

    import edu.knuca.resmat.core.CrossSectionProblemAnswer.{Mapping => M}

    private val steps = Seq(
      TaskFlowStepConf(-1, -1, -1, "Визначення геометричних характеристик окремих елементів складеного перерізу",
        TaskFlowStepType.GroupedInputSet, GroupedInputSetConf(
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
          groupGraphSettings = Some(GeometryShapeInGroupSettingsJson(Some(ShapeCustomAxesSettings("y0", "z0", root = Some(XYCoords(0, 0)))))),
          groupShapeGraphSettings = Some(GeometryShapeInGroupSettingsJson(Some(ShapeCustomAxesSettings("y", "z"))))
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, -1, "Характеристики фігур",
        TaskFlowStepType.DynamicTable, M.shapeCalculatedDataDynamicTable.asJson.toString(), None, true
      ),
      TaskFlowStepConf(-1, -1, -1, "Визначення центру ваги складеного поперечного перерізу в системі координат $ y_0 {O_z}_0 $",
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
      TaskFlowStepConf(-1, -1, -1, "Координати центру ваги і центрів фігур",
        TaskFlowStepType.EquationSetHelp, InputSetEquationSystem(
          "", List(
            InputSetEquation(1, List[EquationItem](
              es("Координати центру ваги: "), es("$y_c = $"), ed(M.y_center, ", ", "", 2), es("$z_c = $"), ed(M.z_center, "", "", 2)
            )),
            InputSetEquation(1, List[EquationItem](
              EquationItem(SmartValueForEach(
                M.shapeIdsDividedByComma,
                Seq(
                  esd(M.Input.nameKey),
                  ed(M.Input.idKey, "", "C", 0),
                  ed(M.Input.yCenterKey, ", ", "(", 2),
                  ed(M.Input.zCenterKey, ")", "", 2)
                ),
                Seq(es(""), es(""), es(""))
              ))
            ))
          )
        ).asJson.toString(),
        None,
        isHelpStep = true
      ),
      TaskFlowStepConf(-1, -1, -1, "Перевірка положення центральної системи координат",
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
                    ei(-1, M.Input.squareKey, "", "", "A"),
                    es("*"),
                    ei(-1, M.aKey, "", "", "a")
                  ),
                  Seq(es("+"))
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
                    ei(-1, M.Input.squareKey, "", "", "A"),
                    es("*"),
                    ei(-1, M.bKey, "", "", "b")
                  ),
                  Seq(es("+"))
                )),
                es("="),
                ei(-1, M.S_z_c, "[$ см^3 $] точність 0,01")
              )
            )
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, -1, "На основі формул паралельного переходу визначаємо центральні моменти інерції складеного поперечного перерізу",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ {I_y}_c = \\sum_{i=0}^n ({I_y}_i + A_i * {a_i}^2) =\\;($"),
                EquationItem(SmartValueForEach(
                  M.shapeIdsDividedByComma,
                  Seq(
                    ei(-1, M.Input.iyKey, "", "", "Iy"),
                    es("+"),
                    ei(-1, M.Input.squareKey, "", "", "A"),
                    es("*"),
                    ei(-1, M.aKey, "", "", "a")
                  ),
                  Seq(es("${}^2)\\;+\\;($"))
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
                    ei(-1, M.Input.izKey, "", "", "Iz"),
                    es("+"),
                    ei(-1, M.Input.squareKey, "", "", "A"),
                    es("*"),
                    ei(-1, M.bKey, "", "", "b")
                  ),
                  Seq(es("${}^2)\\;+\\;($"))
                )),
                es("${}^2)\\;=$"),
                ei(-1, M.I_zc, "[$ см^4 $] точність 0,001")
              )
            )
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, -1, "Визначаємо відцентровий момент інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_{{y_c}{z_c}} = \\sum_{i=0}^n ({I_{yz}}_i + A_i * a_i * b_i) =\\;($"),
                EquationItem(SmartValueForEach(
                  M.shapeIdsDividedByComma,
                  Seq(
                    ei(-1, M.Input.iyzKey, "", "", "Iyz"),
                    es("+"),
                    ei(-1, M.Input.squareKey, "", "", "A"),
                    es("*"),
                    ei(-1, M.aKey, "", "", "a"),
                    es("*"),
                    ei(-1, M.bKey, "", "", "b")
                  ),
                  Seq(es("$)\\;+\\;($"))
                )),
                es("$)\\;=$"),
                ei(-1, M.I_yzc, "[$ см^4 $] точність 0,001")
              )
            )
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, -1, "Визначаємо положення головних центральних осей інерції",
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
      TaskFlowStepConf(-1, -1, -1, "Визначення головних центральних моментів інерції (формули повороту)",
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
      TaskFlowStepConf(-1, -1, -1, "Перевірка головних центральних моментів інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_{max} = \\frac{I_{y_c} + I_{z_c}}{2} + \\sqrt { (\\frac{I_{y_c} - I_{z_c}}{2})^2 + {I_{y_cz_c}}^2 } = $"),
                ei(-1, M.I_max, "$[см^4]$ точність 0,001")
              )
            ),
            InputSetEquation(
              1, List[EquationItem](
                es("$ I_{min} = \\frac{I_{y_c} + I_{z_c}}{2} - \\sqrt { (\\frac{I_{y_c} - I_{z_c}}{2})^2 + {I_{y_cz_c}}^2 } = $"),
                ei(-1, M.I_min, "$[см^4]$ точність 0,001")
              )
            )
          )
        ).asJson.toString()
      ),
      TaskFlowStepConf(-1, -1, -1, "Головні радіуси інерції",
        TaskFlowStepType.EquationSet, InputSetEquationSystem(
          "", List(
            InputSetEquation(
              1, List[EquationItem](
                es("$ i_u = \\sqrt \\frac{I_u}{A} = $"),
                ei(-1, M.i_u, "$[см]$ точність 0,001")
              )
            ),
            InputSetEquation(
              1, List[EquationItem](
                es("$ i_v = \\sqrt \\frac{I_v}{A} = $"),
                ei(-1, M.i_v, "$[см]$ точність 0,001")
              )
            )
          )
        ).asJson.toString(),
        Some(0.1)
      ),
      TaskFlowStepConf(-1, -1, -1, "Фінальне креслення",
        TaskFlowStepType.Drawing, DrawingConf(
          "Фінальне креслення",
          M.finalDrawingShapes,
          Some(GeometryShapeInGroupSettingsJson(Some(ShapeCustomAxesSettings("y0", "z0", Some(XYCoords(0, 0))))))
        ).asJson.toString(),
        isResultInfoStep = true
      ),
      TaskFlowStepConf(-1, -1, -1, "Кінець", TaskFlowStepType.Finished, "{}")
    ).zipWithIndex
      .map(stepWithIndex => stepWithIndex._1.copy(sequence = stepWithIndex._2 + 1))

    val taskFlowConf: TaskFlowConfDto = TaskFlowConfDto(
      TaskFlowConf(-1, 2, "Геометрія"),
      steps
    )
  }


}
