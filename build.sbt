//

val MyScalaVersion = "2.13.4"
val MyScalaCrossVersions = Seq("2.12.10")
val CirceVersion = "0.14.0-M4"

lazy val `klib-core` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib-core"))
    .settings(
      name := "klib-core",
      organization := "kalin-rudnicki",
      version := "0.1.0",
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
      ),
      scalaVersion := MyScalaVersion,
      crossScalaVersions := MyScalaCrossVersions :+ MyScalaVersion,
      resolvers += Resolver.mavenLocal,
    )
    .jsSettings()
    .jvmSettings(
      libraryDependencies ++=
        Seq(
          "org.rogach" %% "scallop" % "4.0.1",
        ) ++
          Seq(
            "org.scalatest" %% "scalatest" % "3.2.3",
          ).map(_ % Test),
    )

lazy val `klib-core-js` = `klib-core`.js
lazy val `klib-core-jvm` = `klib-core`.jvm

lazy val `klib-webServer` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib-webServer"))
    .settings(
      name := "klib-webServer",
      organization := "kalin-rudnicki",
      version := "0.0.1",
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
        "io.circe" %% "circe-core" % CirceVersion,
        "io.circe" %% "circe-generic" % CirceVersion,
        "io.circe" %% "circe-parser" % CirceVersion,
      ),
      scalaVersion := MyScalaVersion,
      crossScalaVersions := MyScalaCrossVersions :+ MyScalaVersion,
      resolvers += Resolver.mavenLocal,
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "1.1.0",
      ),
    )
    .jvmSettings()

lazy val `klib-webServer-js` = `klib-webServer`.js.dependsOn(`klib-core-js`)
lazy val `klib-webServer-jvm` = `klib-webServer`.jvm.dependsOn(`klib-core-jvm`)
