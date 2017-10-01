package edu.knuca.resmat.utils

import anorm.SQL

object SqlUtils {
  def get(tableName: String) = SQL(s"SELECT * FROM $tableName")
  def get(tableName: String, id: Long) = SQL(s"SELECT * FROM $tableName WHERE id = {id}").on("id" -> id)
  def delete(tableName: String, id: Long) = SQL(s"DELETE FROM $tableName WHERE id = {id}").on("id" -> id)
}
