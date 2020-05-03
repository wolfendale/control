name := "control"

version := "0.1"

scalaVersion := "2.13.2"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.1.1"
libraryDependencies += "io.monix" %% "monix" % "3.2.1"
