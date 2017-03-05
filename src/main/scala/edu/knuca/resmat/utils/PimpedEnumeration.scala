package edu.knuca.resmat.utils

abstract class PimpedEnumeration extends Enumeration {

  case class EnumValue(id: Int, name: String)

  def valueSet: Set[EnumValue] = super.values.map(v => EnumValue(v.id, v.toString))
}
