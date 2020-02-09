package edu.knuca.resmat.data

import edu.knuca.resmat.exam._
import edu.knuca.resmat.exam.taskflow.TaskFlowConfAndExamService

object Utils {
  def ei(id: Int, answerMapping: String, suffix: String = ""): EquationItem = EquationItem(SmartValueInput(id, answerMapping), "", suffix)
  def es(value: String): EquationItem = EquationItem(SmartValueStaticString(value))
  def ed(answerMapping: String, suffix: String = ""): EquationItem = EquationItem(SmartValueDynamicDouble(answerMapping), "", suffix)
}

class TaskFlowDataGenerator(taskFlowConfAndExamService: TaskFlowConfAndExamService) {
  val ringPlateNotInsertedTaskFlowConfDto: TaskFlowConfDto = RingPlateData.TaskFlow.taskFlowConf
  val crossSectionNotInsertedTaskFlowConfDto: TaskFlowConfDto = CrossSectionData.TaskFlow.taskFlowConf
}