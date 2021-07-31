//

val Scala_2_12 = "2.12.10"
val Scala_2_13 = "2.13.4"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "klib"

ThisBuild / dynverVTagPrefix := false
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / version ~= (_.replace('+', '-'))
ThisBuild / dynver ~= (_.replace('+', '-'))

// =====|  |=====

inThisBuild(
  Seq(
    organization := MyOrg,
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("public"),
    ),
    //
    description := "My personal FP library for scala.",
    licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
    homepage := Some(url(s"https://github.com/$githubUsername/$githubProject")),
    developers := List(
      Developer(
        id = "Kalin-Rudnicki",
        name = "Kalin Rudnicki",
        email = "kalin.rudnicki@gmail.com",
        url = url(s"https://github.com/$githubUsername"),
      ),
    ),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
  ),
)

// =====|  |=====

lazy val `klib-core` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib-core"))
    .settings(
      name := "klib-core",
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
      ),
      scalaVersion := Scala_2_13,
      crossScalaVersions := Seq(Scala_2_12, Scala_2_13),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .jsSettings()
    .jvmSettings(
      libraryDependencies ++=
        Seq(
          "org.rogach" %% "scallop" % "4.0.4",
        ) ++
          Seq(
            "org.scalatest" %% "scalatest" % "3.2.3",
          ).map(_ % Test),
    )

lazy val `klib-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .aggregate(
      `klib-core`.js,
      `klib-core`.jvm,
    )
