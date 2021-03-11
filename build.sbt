name := "control"

version := "0.1"

scalaVersion := "2.13.2"
scalacOptions += "-Ymacro-annotations"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.1.1"
libraryDependencies += "io.monix" %% "monix" % "3.2.1"
libraryDependencies += "io.iteratee" %% "iteratee-core" % "0.19.0"
//libraryDependencies += "io.higherkindness" %% "droste-core" % "0.8.0"
//libraryDependencies += "io.higherkindness" %% "droste-macros" % "0.8.0"

