name := "resmat"

organization := "edu.knuca"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in assembly := Some("edu.knuca.resmat.Api")

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
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
    "com.papertrailapp" % "logback-syslog4j" % "1.0.0"
  )
}

val dbDependencies = Seq(
  "com.h2database" % "h2" % "1.4.177",
  "mysql" % "mysql-connector-java" % "6.0.5",
  "org.flywaydb" % "flyway-core" % "3.2.1",
  "com.zaxxer" % "HikariCP" % "2.4.5",
  "com.typesafe.play" %% "anorm" % "2.5.0"
)

val mathDependencies = {
  val breezeVersion = "0.12"
  Seq(
    "org.scalanlp" %% "breeze" % breezeVersion,
    "org.scalanlp" %% "breeze-natives" % breezeVersion
  )
}

libraryDependencies ++= {
  val akkaV = "10.0.3"
  val scalaTestV = "3.0.1"
  val akkaHttpCorsV = "0.1.11"
  val awsSdkV = "1.11.172"
  Seq(
    "com.typesafe.akka" %% "akka-http-core" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaV,

    "ch.megard" %% "akka-http-cors" % akkaHttpCorsV,
    "com.amazonaws" % "aws-java-sdk" % awsSdkV,

    "org.scalatest" %% "scalatest" % scalaTestV % "test",
    "com.typesafe.akka" %% "akka-http-testkit" % akkaV % "test"
  )
}

libraryDependencies ++=
  jsonDependencies ++
  loggingDependencies ++
  dbDependencies ++
  mathDependencies

assemblyMergeStrategy in assembly := {
  case PathList("application.conf") => MergeStrategy.discard
  case PathList("flyway.conf") => MergeStrategy.discard
  case PathList("logback.xml") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)


