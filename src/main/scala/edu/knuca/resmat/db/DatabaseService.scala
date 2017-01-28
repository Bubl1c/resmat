package edu.knuca.resmat.db

import java.sql.Connection

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

case class DBConfig(jdbcUrl: String, user: String, password: String, driver: String, maxConnections: Int)

trait DatabaseService {
  def dbConfig: DBConfig

  Class.forName(dbConfig.driver).newInstance

  private val hikariConfig = new HikariConfig()
  hikariConfig.setJdbcUrl(dbConfig.jdbcUrl)
  hikariConfig.setUsername(dbConfig.user)
  hikariConfig.setPassword(dbConfig.password)
  hikariConfig.setMaximumPoolSize(dbConfig.maxConnections)

  private val dataSource = new HikariDataSource(hikariConfig)

  def run[A](block: Connection => A): A = {
    val conn = dataSource.getConnection()
    try {
      conn.setAutoCommit(true)
      block(conn)
    } finally {
      conn.close()
    }
  }

  def runTransaction[A](block: Connection => A): A = {
    run { connection =>
      try {
        connection.setAutoCommit(false)
        val r = block(connection)
        connection.commit()
        r
      } catch {
        case e: Throwable => connection.rollback(); throw e
      }
    }
  }

  def close(): Unit = dataSource.close()

}
