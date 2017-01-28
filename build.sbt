name := "resmat"

organization := "edu.knuca"

version := "1.0"

scalaVersion := "2.11.8"

val jsonDependencies = {
  val circeV = "0.7.0"
  Seq(
    "io.circe" %% "circe-core" % circeV,
    "io.circe" %% "circe-generic" % circeV,
    "io.circe" %% "circe-parser" % circeV,
    "de.heikoseeberger" %% "akka-http-circe" % "1.11.0"
  )
}

val loggingDependencies = {
  val logbackV = "1.1.9"
  Seq(
    "ch.qos.logback" % "logback-core" % logbackV,
    "ch.qos.logback" % "logback-classic" % logbackV,
    "ch.qos.logback" % "logback-access" % logbackV,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
  )
}

val dbDependencies = Seq(
  "com.h2database" % "h2" % "1.4.177",
  "mysql" % "mysql-connector-java" % "6.0.5",
  "org.flywaydb" % "flyway-core" % "3.2.1",
  "com.zaxxer" % "HikariCP" % "2.4.5",
  "com.typesafe.play" %% "anorm" % "2.5.0"
)


libraryDependencies ++= {
  val akkaV = "10.0.3"
  val scalaTestV = "3.0.1"
  Seq(
    "com.typesafe.akka" %% "akka-http-core" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaV,

    "org.scalatest" %% "scalatest" % scalaTestV % "test",
    "com.typesafe.akka" %% "akka-http-testkit" % akkaV % "test"
  )
}

libraryDependencies ++=
  jsonDependencies ++
  loggingDependencies ++
  dbDependencies

    