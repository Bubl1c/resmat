package edu.knuca.resmat.db

import org.flywaydb.core.Flyway

class FlywayService(jdbcUrl: String,
                    dbUser: String,
                    dbPassword: String,
                    schemaName: String,
                    baselineVersion: String) {

  private[this] val flyway = new Flyway()
  flyway.setDataSource(jdbcUrl, dbUser, dbPassword)
  flyway.setSchemas(schemaName)
  flyway.setBaselineVersionAsString(baselineVersion)

  def migrateDatabaseSchema() : Unit = flyway.migrate()

  def dropDatabase() : Unit = flyway.clean()

  def repair() : Unit = flyway.repair()
}
