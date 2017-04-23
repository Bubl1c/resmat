package edu.knuca.resmat.utils

import com.typesafe.config.ConfigFactory

import scala.util.Try

trait Config {
  private val _config = ConfigFactory.load()
  private val httpConfig = _config.getConfig("http")
  private val databaseConfig = _config.getConfig("database")

  val httpHost = httpConfig.getString("interface")
  val httpPort = httpConfig.getInt("port")

  object MySql {
    private lazy val config = _config.getConfig("mysql-database")

    val options = "jdbcCompliantTruncation=false&characterEncoding=UTF-8&serverTimezone=GMT&useSSL=false"
    val host = config.getString("host")
    val port = config.getString("port")
    val db = config.getString("database")
    val user = config.getString("user")
    val password = config.getString("password")
    val conns = Try(config.getInt("connections")).getOrElse(20)
    val driver = config.getString("driver")
    val jdbcUrl = s"jdbc:mysql://$host:$port/$db?$options"
    val flywayJdbcUrl = s"jdbc:mysql://$host:$port/mysql?$options"
  }
}