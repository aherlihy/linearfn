ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "linearfn",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
