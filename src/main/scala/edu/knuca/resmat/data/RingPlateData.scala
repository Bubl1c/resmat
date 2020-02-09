package edu.knuca.resmat.data

import edu.knuca.resmat.core.ringplate.RingPlateSolver
import edu.knuca.resmat.exam.{EquationItem, InputSet, InputSetEquation, InputSetEquationSystem, InputSetInput, ProblemConf, ProblemInputVariableValue, ProblemType, ProblemVariantConf, ProblemVariantSchemaType, TaskFlowConf, TaskFlowConfDto, TaskFlowStepConf, TaskFlowStepType, TaskFlowTestConf, TestConf, TestOptionConf}
import io.circe.generic.auto._
import io.circe.syntax._
import edu.knuca.resmat.http.JsonProtocol._

object RingPlateData {

  object Problem {

    import edu.knuca.resmat.exam.{ProblemInputVariableConf => VarConf, ProblemInputVariableValue => VarVal}

    private val problemVariableConfs = Seq(
      VarConf(2, "Fa", "кН/м", "a.f"),
      VarConf(3, "Ma", "кНм/м", "a.m"),
      VarConf(4, "wa", "м", "a.w"),
      VarConf(5, "{phi}a", "рад", "a.fi"),

      VarConf(6, "E", "МПа", "moduleE"),
      VarConf(7, "{mu}", "", "poissonRatio"),
      VarConf(8, "q", "кН/м^2", "q"),

      VarConf(9, "Fb", "кН/м", "b.f"),
      VarConf(10, "Mb", "кНм/м", "b.m"),
      VarConf(11, "wb", "м", "b.w"),
      VarConf(12, "{phi}b", "рад", "b.fi"),

      VarConf(13, "a", "м", "a.length"),
      VarConf(14, "b", "м", "b.length"),
      VarConf(15, "t", "м", "height"),

      VarConf(16, "an", "", "a.n", false),
      VarConf(17, "bn", "", "b.n", false),
      VarConf(18, "{sigma}адм", "", "sigmaAdm")
    )

    private val problemVarValues: List[ProblemInputVariableValue] = List(
      VarVal(2, 0),
      VarVal(3, 0),
      VarVal(4, -0.01),
      VarVal(5, 0),

      VarVal(6, 200000.0),
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

    private val problemVarValues2: List[ProblemInputVariableValue] = List(
      VarVal(2, 0),
      VarVal(3, 0),
      VarVal(4, -0.01),
      VarVal(5, 0),

      VarVal(6, 200000.0),
      VarVal(7, 0.3),
      VarVal(8, 0d),

      VarVal(9, 0),
      VarVal(10, 0),
      VarVal(11, 0),
      VarVal(12, 0),

      VarVal(13, 0.1),
      VarVal(14, 1.1),
      VarVal(15, 0.02),

      VarVal(16, 2),
      VarVal(17, 2),
      VarVal(18, 160)
    )

    val conf: ProblemConf = ProblemConf(1, "Кільцева пластина", ProblemType.RingPlate, problemVariableConfs)

    val variants: Seq[ProblemVariantConf] = Seq(
      ProblemVariantConf(
        1, 1, ProblemVariantSchemaType.ImgUrl, "img/tasks/variants/sc1.png",
        problemVarValues,
        new RingPlateSolver(problemVariableConfs, problemVarValues).solve()
      ),
      ProblemVariantConf(
        1, 1, ProblemVariantSchemaType.ImgUrl, "img/tasks/variants/sc2.png",
        problemVarValues2,
        new RingPlateSolver(problemVariableConfs, problemVarValues2).solve()
      )
    )
  }

  object TaskFlow {

    import edu.knuca.resmat.core.RingPlateProblemAnswer.{Mapping => M}
    import edu.knuca.resmat.data.Utils.{ed, ei, es}

    val taskFlowConf: TaskFlowConfDto = TaskFlowConfDto(
      TaskFlowConf(1, 1, "Порядок виконання задачі"), Seq(
        TaskFlowStepConf(
          1, 1, 1, "Визначення типу пластини",
          TaskFlowStepType.Test, TaskFlowTestConf(TestConf(
            -1, -1, "Визначте тип ластини", None, Seq(
              TestOptionConf(1, "Тонкі", true),
              TestOptionConf(2, "Товсті"),
              TestOptionConf(3, "Мембрани")
            )
          )
          ).asJson.toString()
        ),
        TaskFlowStepConf(
          2, 1, 2, "Введіть значення граничних умов, якщо умова невідома - залиште поле пустим",
          TaskFlowStepType.InputSet, InputSet(
            1, "InputSetName", Seq(
              InputSetInput(1, "w(a)", "На внутрішньому контурі", "м", M.w_a),
              InputSetInput(2, "{phi}(a)", "На внутрішньому контурі", "рад", M.fi_a),
              InputSetInput(3, "Mr(a)", "На внутрішньому контурі", "кНм/м", M.mr_a),
              InputSetInput(4, "Qr(a)", "На внутрішньому контурі", "кН/м", M.qr_a),
              InputSetInput(5, "w(b)", "На зовнішньому контурі", "м", M.w_b),
              InputSetInput(6, "{phi}(b)", "На зовнішньому контурі", "рад", M.fi_b),
              InputSetInput(7, "Mr(b)", "На зовнішньому контурі", "кНм/м", M.mr_b),
              InputSetInput(8, "Qr(b)", "На зовнішньому контурі", "кН/м", M.qr_b)
            )
          ).normalised.asJson.toString()
        ),
        TaskFlowStepConf(
          3, 1, 3, "Значення граничних умов",
          TaskFlowStepType.VariableValueSet, InputSet(
            1, "InputSetName", Seq(
              InputSetInput(1, "w(a)", "На внутрішньому контурі", "м", M.w_a),
              InputSetInput(2, "{phi}(a)", "На внутрішньому контурі", "рад", M.fi_a),
              InputSetInput(3, "Mr(a)", "На внутрішньому контурі", "кНм/м", M.mr_a),
              InputSetInput(4, "Qr(a)", "На внутрішньому контурі", "кН/м", M.qr_a),
              InputSetInput(5, "w(b)", "На зовнішньому контурі", "м", M.w_b),
              InputSetInput(6, "{phi}(b)", "На зовнішньому контурі", "рад", M.fi_b),
              InputSetInput(7, "Mr(b)", "На зовнішньому контурі", "кНм/м", M.mr_b),
              InputSetInput(8, "Qr(b)", "На зовнішньому контурі", "кН/м", M.qr_b)
            )
          ).normalised.asJson.toString(),
          None,
          true
        ),
        TaskFlowStepConf(
          4, 1, 4, "Циліндрична жорсткість",
          TaskFlowStepType.InputSet, InputSet(
            2, "Циліндрична жорсткість", Seq(
              InputSetInput(1, "D", "", "", M.d_e)
            )
          ).normalised.asJson.toString()
        ),
        TaskFlowStepConf(
          5, 1, 5, "Заповніть коефіцієнти системи рівнянь",
          TaskFlowStepType.EquationSet, InputSetEquationSystem(
            "Заповніть коефіцієнти системи рівнянь", List(
              InputSetEquation(
                1, List[EquationItem](
                  ed(M.g1_00, "X1"), es("+"), ei(12, M.g1_01, "X2"), es("+"), ei(13, M.g1_02, "X3"), es("+"), ei(14, M.g1_03, "X4"), es("="), ed(M.g1_04)
                )
              ),
              InputSetEquation(
                2, List[EquationItem](
                  ed(M.g1_10, "X1"), es("+"), ei(22, M.g1_11, "X2"), es("+"), ei(23, M.g1_12, "X3"), es("+"), ei(24, M.g1_13, "X4"), es("="), ed(M.g1_14)
                )
              ),
              InputSetEquation(
                3, List[EquationItem](
                  ed(M.g1_20, "X1"), es("+"), ei(32, M.g1_21, "X2"), es("+"), ei(33, M.g1_22, "X3"), es("+"), ei(34, M.g1_23, "X4"), es("="), ed(M.g1_24)
                )
              ),
              InputSetEquation(
                4, List[EquationItem](
                  ed(M.g1_30, "X1"), es("+"), ei(42, M.g1_31, "X2"), es("+"), ei(43, M.g1_32, "X3"), es("+"), ei(44, M.g1_33, "X4"), es("="), ed(M.g1_34)
                )
              )
            )
          ).asJson.toString()
        ),
        TaskFlowStepConf(
          6, 1, 6, "Сталі інтегрування:",
          TaskFlowStepType.VariableValueSet, InputSet(
            1, "Сталі інтегрування:", Seq(
              InputSetInput(1, "X1", "", "м", M.x1),
              InputSetInput(2, "X2", "", "рад", M.x2),
              InputSetInput(3, "X3", "", "кНм/м", M.x3),
              InputSetInput(4, "X4", "", "кН/м", M.x4)
            )
          ).normalised.asJson.toString(),
          None,
          true
        ),
        TaskFlowStepConf(
          7, 1, 7, "Введіть значення шуканих функцій в перерізі на відстані 0,2 м від лівої опори.",
          TaskFlowStepType.InputSet, InputSet(
            2, "Введіть значення шуканих функцій в перерізі на відстані 0,2 м від лівої опори.", Seq(
              InputSetInput(1, "w", "", "м", M.w1_2),
              InputSetInput(2, "{phi}", "", "рад", M.fi1_2),
              InputSetInput(3, "Mr", "", "кНм/м", M.mr1_2),
              InputSetInput(4, "Qr", "", "кН/м", M.qr1_2)
            )
          ).normalised.asJson.toString()
        ),
        TaskFlowStepConf(8, 1, 8, "Епюри", TaskFlowStepType.Charts, M.charts.asJson.toString(), None, true),
        TaskFlowStepConf(
          9, 1, 9, "Введіть пораховані значення",
          TaskFlowStepType.InputSet, InputSet(
            3, "InputSetName", Seq(
              InputSetInput(1, "r", "Координати небезпечного перерізу", "м", M.r),
              InputSetInput(2, "{sigma}r", "Радіального нормального напруження", "МПа", M.sigma_r),
              InputSetInput(3, "{sigma}{theta}", "Колового нормального напруження", "МПа", M.sigma_theta),
              InputSetInput(4, "{sigma}екв", "Еквівалентного нормального напруження", "МПа", M.sigma_eq),
              InputSetInput(5, "{tau}max", "Максимальних дотичних напружень", "МПа", M.tau_max)
            )
          ).normalised.asJson.toString()
        ),
        TaskFlowStepConf(
          10, 1, 10, "Чи забезпечується міцність перерізу?",
          TaskFlowStepType.Test, TaskFlowTestConf(
            TestConf(
              -1, -1, "Чи забезпечуться міцність перерізу?", None, Seq(
                TestOptionConf(0, "Не забезпечується"),
                TestOptionConf(1, "Забезпечується")
              )
            ), Some(M.isStrengthGuranteed)
          ).asJson.toString()
        ),
        TaskFlowStepConf(11, 1, 11, "Кінець", TaskFlowStepType.Finished, "{}"),
        TaskFlowStepConf(
          12, 1, 12, "Епюри в табличному вигляді",
          TaskFlowStepType.DynamicTable, M.chartsAsTable.asJson.toString(), None, false, true
        )
      )
    )
  }

}
