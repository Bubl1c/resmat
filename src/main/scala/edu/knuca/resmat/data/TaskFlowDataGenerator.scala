package edu.knuca.resmat.data

import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.taskflow.TaskFlowConfAndExamService

object Utils {
  def ei(id: Int, answerMapping: String, suffix: String = "", prefix: String = "", labelKey: String = ""): EquationItem = EquationItem(SmartValueInput(id, answerMapping, labelKey), prefix, suffix)
  def es(value: String, prefix: String = "", suffix: String = ""): EquationItem = EquationItem(SmartValueStaticString(value), prefix, suffix)
  def esd(answerMapping: String, prefix: String = "", suffix: String = ""): EquationItem = EquationItem(SmartValueDynamicString(answerMapping), prefix, suffix)
  def ed(answerMapping: String, suffix: String = "", prefix: String = "", digitsAfterComma: Int = 6): EquationItem = EquationItem(SmartValueDynamicDouble(answerMapping, 0.0, digitsAfterComma), prefix, suffix)
}

class TaskFlowDataGenerator(taskFlowConfAndExamService: TaskFlowConfAndExamService) {
  val ringPlateNotInsertedTaskFlowConfDto: TaskFlowConfDto = RingPlateData.TaskFlow.taskFlowConf
  val crossSectionNotInsertedTaskFlowConfDto: TaskFlowConfDto = CrossSectionData.TaskFlow.taskFlowConf
}