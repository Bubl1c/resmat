package edu.knuca.resmat.data

import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.taskflow.TaskFlowExamService
import io.circe.generic.auto._
import io.circe.syntax._

object TaskFlowData {
  import edu.knuca.resmat.core.RingPlateProblemAnswer.{Mapping => M}

  import edu.knuca.resmat.http.JsonProtocol._

  def ei(id: Int, answerMapping: String, suffix: String = "") = EquationItem(EquationItemInput(id, answerMapping), "", suffix)
  def es(value: String) = EquationItem(EquationItemStaticStringValue(value))
  def ed(answerMapping: String) = EquationItem(EquationItemDynamicDoubleValue(answerMapping))

  val taskFlows: Seq[(TaskFlowConf, Seq[TaskFlowStepConf])] = Seq(
    (TaskFlowConf(1, 1, "Пацанський порядок виконання задачі"), Seq(
      TaskFlowStepConf(1, 1, "Визначення типу пластини", 1, TaskFlowStepType.Test,
        TaskFlowTestConf(TestConf(-1, -1, "Визначте тип ластини", None, Seq(
          TestOptionConf(1, "Тонкі", true),
          TestOptionConf(2, "Товсті"),
          TestOptionConf(3, "Мембрани")
        ))).asJson.toString()
      ),
      TaskFlowStepConf(2, 1, "Введіть значення граничних умов, якщо умова невідома - залиште поле пустим", 2,
        TaskFlowStepType.InputSet, InputSet(1, "InputSetName", Seq(
          InputSetInput(1, "w(a)", "На внутрішньому контурі", "м", M.w_a),
          InputSetInput(2, "{phi}(a)", "На внутрішньому контурі", "рад", M.fi_a),
          InputSetInput(3, "Mr(a)", "На внутрішньому контурі", "кНм/м", M.mr_a),
          InputSetInput(4, "Qr(a)", "На внутрішньому контурі", "кН/м", M.qr_a),
          InputSetInput(5, "w(b)", "На зовнішньому контурі", "м", M.w_b),
          InputSetInput(6, "{phi}(b)", "На зовнішньому контурі", "рад", M.fi_b),
          InputSetInput(7, "Mr(b)", "На зовнішньому контурі", "кНм/м", M.mr_b),
          InputSetInput(8, "Qr(b)", "На зовнішньому контурі", "кН/м", M.qr_b)
        )).normalised.asJson.toString()
      ),
      TaskFlowStepConf(3, 1, "Значення граничних умов", 3,
        TaskFlowStepType.VariableValueSet, InputSet(1, "InputSetName", Seq(
          InputSetInput(1, "w(a)", "На внутрішньому контурі", "м", M.w_a),
          InputSetInput(2, "{phi}(a)", "На внутрішньому контурі", "рад", M.fi_a),
          InputSetInput(3, "Mr(a)", "На внутрішньому контурі", "кНм/м", M.mr_a),
          InputSetInput(4, "Qr(a)", "На внутрішньому контурі", "кН/м", M.qr_a),
          InputSetInput(5, "w(b)", "На зовнішньому контурі", "м", M.w_b),
          InputSetInput(6, "{phi}(b)", "На зовнішньому контурі", "рад", M.fi_b),
          InputSetInput(7, "Mr(b)", "На зовнішньому контурі", "кНм/м", M.mr_b),
          InputSetInput(8, "Qr(b)", "На зовнішньому контурі", "кН/м", M.qr_b)
        )).normalised.asJson.toString(),
        true
      ),
      TaskFlowStepConf(4, 1, "Введіть пораховані значення коефіцієнтів рівняння", 4,
        TaskFlowStepType.EquationSet, InputSetEquationSystem("InputSetEquationSystem", List(
          InputSetEquation(1, List[EquationItem](
            ei(1, M.g1_00, "X1"), es("+"), ei(1, M.g1_01, "X2"), es("+"), ei(1, M.g1_02, "X3"), es("+"), ei(1, M.g1_03, "X4")
          ), List(
            ed(M.g1_04)
          )),
          InputSetEquation(2, List[EquationItem](
            ei(1, M.g1_10, "X1"), es("+"), ei(1, M.g1_11, "X2"), es("+"), ei(1, M.g1_12, "X3"), es("+"), ei(1, M.g1_13, "X4")
          ), List(
            ed(M.g1_14)
          )),
          InputSetEquation(3, List[EquationItem](
            ei(1, M.g1_20, "X1"), es("+"), ei(1, M.g1_21, "X2"), es("+"), ei(1, M.g1_22, "X3"), es("+"), ei(1, M.g1_23, "X4")
          ), List(
            ed(M.g1_24)
          )),
          InputSetEquation(4, List[EquationItem](
            ei(1, M.g1_30, "X1"), es("+"), ei(1, M.g1_31, "X2"), es("+"), ei(1, M.g1_32, "X3"), es("+"), ei(1, M.g1_33, "X4")
          ), List(
            ed(M.g1_34)
          ))
        )).asJson.toString()
      ),
//      TaskFlowStepConf(4, 1, "Введіть пораховані значення невідомих X", 4,
//        TaskFlowStepType.InputSet, InputSet(2, "InputSetName", Seq(
//          InputSetInput(1, "X1", "", "м", M.x1),
//          InputSetInput(2, "X2", "", "рад", M.x2),
//          InputSetInput(3, "X3", "", "кНм/м", M.x3),
//          InputSetInput(4, "X4", "", "кН/м", M.x4)
//        )).normalised.asJson.toString()
//      ),
      TaskFlowStepConf(5, 1, "Епюри", 5, TaskFlowStepType.Charts, M.charts.asJson.toString(), true),
      TaskFlowStepConf(6, 1, "Введіть пораховані значення", 6,
        TaskFlowStepType.InputSet, InputSet(3, "InputSetName", Seq(
          InputSetInput(1, "r", "Координати небезпечного перерізу", "м", M.r),
          InputSetInput(2, "{sigma}r", "Радіального нормального напруження", "МПа", M.sigma_r),
          InputSetInput(3, "{sigma}{theta}", "Колового нормального напруження", "МПа", M.sigma_theta),
          InputSetInput(4, "{sigma}екв", "Еквівалентного нормального напруження", "МПа", M.sigma_eq),
          InputSetInput(5, "{tau}max", "Максимальних дотичних напружень", "МПа", M.tau_max)
        )).normalised.asJson.toString()
      ),
      TaskFlowStepConf(7, 1, "Чи забезпечується міцність перерізу?", 7, TaskFlowStepType.Test,
        TaskFlowTestConf(TestConf(-1, -1, "Чи забезпечуться міцність перерізу?", None, Seq(
          TestOptionConf(0, "Не забезпечується"),
          TestOptionConf(1, "Забезпечується")
        )), Some(M.isStrengthGuranteed)).asJson.toString()
      ),
      TaskFlowStepConf(8, 1, "Кінець", 8, TaskFlowStepType.Finished, "{}"))
    )
  )
}

class TaskFlowDataGenerator(taskFlowExamService: TaskFlowExamService) {
  private val taskFlowsWithSteps = TaskFlowData.taskFlows.map{ case(tfConf, tfsConfs) =>
    val createdTfc = taskFlowExamService.createTaskFlowConf(tfConf)
    val createdTfsConfs = tfsConfs.map( tfsc =>
      taskFlowExamService.createTaskFlowStepConf(
        tfsc.copy(taskFlowConfId = createdTfc.id)
      )
    )
    (createdTfc, createdTfsConfs)
  }

  val taskFlowConfs: Seq[TaskFlowConf] = taskFlowsWithSteps.map(_._1)

  val taskFlowStepConfs: Seq[TaskFlowStepConf] = taskFlowsWithSteps.flatMap(_._2)
}
