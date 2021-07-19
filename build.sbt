//

val Scala_2_12 = "2.12.10"
val Scala_2_13 = "2.13.4"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "slyce-fp"

ThisBuild / dynverVTagPrefix := false
ThisBuild / dynverSonatypeSnapshots := true

ThisBuild / version ~= (_.replace('+', '-'))
ThisBuild / dynver ~= (_.replace('+', '-'))

// =====|  |=====

inThisBuild(
  Seq(
    organization := MyOrg,
    resolvers += Resolver.mavenLocal,
    //
    description := "My personal FP library for scala.",
    licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
    developers := List(
      Developer(
        id = "Kalin-Rudnicki",
        name = "Kalin Rudnicki",
        email = "kalin.rudnicki@gmail.com",
        url = url(s"https://github.com/$githubUsername"),
      ),
    ),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    // sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    // sonatypeProfileName := "io.github.kalin-rudnicki",
    sonatypePublishTo := {
      val nexus = "https://s01.oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
  ),
)

// =====|  |=====

lazy val `klib-core` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib-core"))
    .settings(
      name := "klib-core",
      organization := MyOrg,
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
      ),
      scalaVersion := Scala_2_13,
      crossScalaVersions := Seq(Scala_2_12, Scala_2_13),
      resolvers += Resolver.mavenLocal,
    )
    .jsSettings()
    .jvmSettings(
      libraryDependencies ++=
        Seq(
          "org.rogach" %% "scallop" % "4.0.3", // TODO (KR) :
        ) ++
          Seq(
            "org.scalatest" %% "scalatest" % "3.2.3",
          ).map(_ % Test),
    )

lazy val `klib-core-js` = `klib-core`.js
lazy val `klib-core-jvm` = `klib-core`.jvm
