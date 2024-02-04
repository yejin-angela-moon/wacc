// name := "wacc-42-compiler"

// version := "1.9.8"

// scalaVersion := "3.3.1"

// assembly / assemblyJarName := "wacc-42-compiler.jar"

// assembly / assemblyOutputPath := file ("./wacc-42-compiler.jar")

Global / onChangedBuildSource := ReloadOnSourceChanges

// This is used by `sbt assembly` to generate your runnable jar file
lazy val sbtAssemblySettings = baseAssemblySettings ++ Seq(
  assembly / assemblyOutputPath := baseDirectory.value / "wacc-42-compiler.jar",
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val root = (project in file(".")).
  settings(
    name := "WACC 42",
    organization := "uk.ac.imperial.doc",
    scalaVersion := "3.3.1",

    sbtAssemblySettings,

    libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.5.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test,

    // Some handy scala compiler flags
    scalacOptions ++= Seq(
        "-deprecation", "-unchecked", "-feature",
        // https://docs.scala-lang.org/overviews/compiler-options/index.html
        "-Xlint:nullary-unit",     // warn when methods without parentheses return Unit
        "-Xlint:infer-any",        // warn when a type argument is inferred to be `Any` (probably a mistake!)
        "-Xlint:unused",           // warn when something has been unused (suppress with `@unused` annotation)
        "-Xlint:nonlocal-return",  // warn when a return statement used an exception for control flow
        "-Wdead-code",             // warn when deadcode is found
        "-Wextra-implicit",        // warn when there are multiple sets of implicit argument brackets
        "-Wnumeric-widen",         // warn when a number is implicitly widened (say `Char` to `Int`): use `.toInt` instead!
    ),
  )