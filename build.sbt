ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"


lazy val root = (project in file("."))
  .settings(
    name := "frontend",
    idePackagePrefix := Some("de.cfaed.sigi"),
    // Enable debugging support
    Compile / run / fork := true,
    Compile / run / javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=localhost:5005"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"

enablePlugins(JavaAppPackaging)


