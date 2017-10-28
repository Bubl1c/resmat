package edu.knuca.resmat.utils

import com.typesafe.config.ConfigFactory

import scala.util.Try

case class ResmatEnv(env: String) {
  if(!Seq("prod", "local").contains(env)) {
    throw new RuntimeException(s"Invalid resmat.env config value '$env'")
  }
  val isProd = env == "prod"
  val isLocal = env == "local"
}

trait Config {
  private val _config = ConfigFactory.load()
  private val httpConfig = _config.getConfig("http")
  private val databaseConfig = _config.getConfig("database")

  object Resmat {
    private lazy val config = _config.getConfig("resmat")

    val env = ResmatEnv(config.getString("env"))
  }

  val requestResultLoggingEnabled: Boolean = Try{_config.getBoolean("logging.requestResultLogs")}.getOrElse(false)

  val httpHost = httpConfig.getString("interface")
  val httpPort = httpConfig.getInt("port")

  object MySql {
    private lazy val config = _config.getConfig("resmat.mysql-database")

    val migrateOnStartup = config.getBoolean("migrateOnStartup")
    val generateDataOnStartup = config.getBoolean("generateDataOnStartup")
    val dropSchemaOnStartup = config.getBoolean("dropSchemaOnStartup")

    val options = "jdbcCompliantTruncation=false&characterEncoding=UTF-8&serverTimezone=GMT&useSSL=false"
    val host = config.getString("host")
    val port = config.getString("port")
    val db = config.getString("database")
    val user = config.getString("user")
    val password = config.getString("password")
    val conns = Try(config.getInt("connections")).getOrElse(20)
    val driver = config.getString("driver")
    val jdbcUrl = s"jdbc:mysql://$host:$port/$db?$options"
    lazy val flywayJdbcUrl = Flyway.jdbcUrl
  }

  object Flyway {
    private lazy val config = ConfigFactory.load("flyway").getConfig("flyway")

    val host = config.getString("host")
    val db = config.getString("database")
    val user = config.getString("user")
    val password = config.getString("password")
    val baselineVersion = config.getString("baselineVersion")
    val jdbcUrl = s"jdbc:mysql://$host:${MySql.port}/mysql?${MySql.options}"
  }
}