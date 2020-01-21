package edu.knuca.resmat.core

trait ProblemAnswer {
  protected val mapping: Map[String, Any]

  def get(key: String): Any = {
    mapping.get(key) match {
      case v: Some[Any] => v.get
      case v => throw new IllegalArgumentException(s"Key {$key} does not exist in ${this.getClass.getSimpleName}")
    }
  }

  def getDouble(key: String): Double = {
    mapping.get(key) match {
      case v: Some[Double] => v.get
      case v => throw new IllegalArgumentException(s"{$v} is not a Double value. Requested from ${this.getClass.getSimpleName} by key {$key}")
    }
  }

  def getDoubleOpt(key: String): Option[Double] = {
    mapping.get(key) match {
      case v: Some[Option[Double]] => v.get
      case v => throw new IllegalArgumentException(s"{$v} is not an Option[Double] value. Requested from ${this.getClass.getSimpleName} by key {$key}")
    }
  }

  def getString(key: String): String = {
    mapping.get(key) match {
      case v: Some[String] => v.get
      case v => throw new IllegalArgumentException(s"{$v} is not a String value. Requested from ${this.getClass.getSimpleName} by key {$key}")
    }
  }
}