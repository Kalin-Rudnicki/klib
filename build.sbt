//

lazy val app = crossProject(JSPlatform, JVMPlatform)
  .settings(
    name := "klib",
    organization := "kalin-rudnicki",
    version := "0.1.0",
    unmanagedSourceDirectories in Compile +=
      baseDirectory.value / "shared" / "main" / "scala",
    libraryDependencies ++= Seq(
    ),
    scalaVersion := "2.13.4",
    crossScalaVersions := Seq("2.12.10", "2.13.4"),
    resolvers += Resolver.mavenLocal,
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.1.0",
    ),
  )
  .jvmSettings(
    libraryDependencies ++=
      Seq(
        "org.rogach" %% "scallop" % "4.0.1",
      ) ++
        Seq(
          "org.scalatest" %% "scalatest" % "3.2.3",
        ).map(_ % Test),
  )

lazy val appJS = app.js
lazy val appJVM = app.jvm.settings(
  (resources in Compile) += (fastOptJS in (appJS, Compile)).value.data,
)
