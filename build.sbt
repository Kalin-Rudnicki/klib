//

val MyScalaVersion = "2.13.4"
val MyScalaCrossVersions = Seq("2.12.10")

lazy val `klib-core` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib-core"))
    .settings(
      name := "klib-core",
      organization := "kalin-rudnicki",
      version := "0.1.6",
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
