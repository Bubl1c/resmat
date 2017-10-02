package edu.knuca.resmat

import edu.knuca.resmat.db.FlywayService
import edu.knuca.resmat.utils.Config

object Migrator extends App with Config {

  val flywayService = new FlywayService(Flyway.jdbcUrl, Flyway.user, Flyway.password, Flyway.db, Flyway.baselineVersion)
  flywayService.migrateDatabaseSchema()

}
