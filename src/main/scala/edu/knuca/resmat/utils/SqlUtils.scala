package edu.knuca.resmat.utils

import anorm.SQL

object SqlUtils {
  def get(tableName: String, id: Long) = SQL(s"SELECT * FROM $tableName WHERE id = {id}").on("id" -> id)
}
