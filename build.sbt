name := "Spinaldevices"

version := "1.0"

scalaVersion := "2.11.12"

EclipseKeys.withSource := true

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "1.3.0-SNAPSHOT",
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "1.3.0-SNAPSHOT"
)

// addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.6" % "1.0.2")
// scalacOptions += "-P:continuations:enable"
fork := true
