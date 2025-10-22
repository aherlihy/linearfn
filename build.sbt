ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "linearfn",
    scalacOptions ++= Seq("-experimental"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    Compile / sourceGenerators += Def.task {
      val sourceDir = (Compile / scalaSource).value
      val targetDir = (Compile / sourceManaged).value
      val log = streams.value.log

      OpsExtensionGenerator.generate(sourceDir, targetDir, log)
    }.taskValue
  )
