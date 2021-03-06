lazy val commonSettings = Seq(
  scalaVersion := "2.13.2",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "control",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.3.0",
      "org.typelevel" %% "cats-effect" % "3.2.2",
    )
  )

lazy val playExample = (project in file("play-example"))
  .enablePlugins(PlayScala)
  .dependsOn(root)
  .settings(
    commonSettings,
    name := "play-example",
    version := "0.1.0",
    routesImport += "wolfendale.example.Identifier",
    libraryDependencies ++= Seq(
      guice,
      "org.typelevel" %% "cats-core" % "2.3.0",
      "org.typelevel" %% "cats-effect" % "3.2.2",
      "com.beachape" %% "enumeratum-play" % "1.6.1"
    )
  )

lazy val ossExample = (project in file("oss-example"))
  .enablePlugins(PlayScala)
  .dependsOn(root)
  .settings(
    commonSettings,
    name := "oss-example",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      guice,
      "org.typelevel" %% "cats-core" % "2.3.0",
      "org.typelevel" %% "cats-effect" % "3.2.2",
    )
  )