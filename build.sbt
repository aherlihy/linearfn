ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "restrictedfn",
    // Note: -experimental removed since @ops is now a simple marker annotation
    // scalacOptions ++= Seq("-experimental"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    // Force tests to run serially to avoid race conditions with global counters
    Test / parallelExecution := false,
    Compile / sourceGenerators += Def.task {
      val sourceDir = (Compile / scalaSource).value
      val targetDir = (Compile / sourceManaged).value
      val log = streams.value.log

      OpsExtensionGenerator.generate(sourceDir, targetDir, log)
    }.taskValue,
    // Also generate extensions for test code
    Test / sourceGenerators += Def.task {
      val sourceDir = (Test / scalaSource).value
      val targetDir = (Test / sourceManaged).value
      val log = streams.value.log

      OpsExtensionGenerator.generate(sourceDir, targetDir, log)
    }.taskValue
  )

lazy val bench = (project in file("bench"))
  .dependsOn(root % "compile->compile;compile->test")
  .enablePlugins(JmhPlugin)
  .settings(
    Jmh/compile := (Jmh/compile).dependsOn(Test/compile).value,
    Jmh/run := (Jmh/run).dependsOn(Jmh/compile).evaluated,

    // sbt-jmh generates a ton of Java files, but they're never referenced by Scala files.
    // By enforcing this using `compileOrder`, we avoid having to run these generated files
    // through the Scala typechecker which has a significant impact on compile-time.
    Jmh/compileOrder := CompileOrder.ScalaThenJava
  )
