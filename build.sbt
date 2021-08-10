name := "control"

version := "0.1"

scalaVersion := "2.13.2"
scalacOptions += "-Ymacro-annotations"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.3.0"
libraryDependencies += "io.monix" %% "monix" % "3.2.1"

