package edu.knuca.resmat.db

import org.flywaydb.core.Flyway

class FlywayService(jdbcUrl: String, dbUser: String, dbPassword: String, schemaName: String) {

  private[this] val flyway = new Flyway()
  flyway.setDataSource(jdbcUrl, dbUser, dbPassword)
  flyway.setSchemas(schemaName)

  def migrateDatabaseSchema() : Unit = flyway.migrate()

  def dropDatabase() : Unit = flyway.clean()
}
