//

// val Scala_2_13 = "2.13.4"
val Scala_3_0 = "3.0.0"

val MyScalaVersion = Scala_3_0
// val MyScalaCrossVersions = Seq(Scala_2_13, Scala_3_0)

lazy val `klib-core` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib-core"))
    .settings(
      name := "klib-core",
      organization := "kalin-rudnicki",
      version := "1.0.0",
      Compile / unmanagedSourceDirectories +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
      ),
      scalaVersion := MyScalaVersion,
      // crossScalaVersions := MyScalaCrossVersions,
      resolvers += Resolver.mavenLocal,
      // TODO (KR) : Convert to using source generator instead
    )
    .jsSettings()
    .jvmSettings(
      libraryDependencies ++=
        Seq(
          "org.rogach" %% "scallop" % "4.0.3",
        ) ++
          Seq(
            "org.scalatest" %% "scalatest" % "3.2.9",
          ).map(_ % Test),
    )

lazy val `klib-core-js` = `klib-core`.js
lazy val `klib-core-jvm` = `klib-core`.jvm
