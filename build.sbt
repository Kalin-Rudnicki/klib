//

val Scala_2_12 = "2.12.10"
val Scala_2_13 = "2.13.4"
val Scala_3 = "3.1.0"
val CirceVersion = "0.15.0-M1" // REMOVE : ...

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
    description := "FP & misc. utils library for Scala.",
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
        "io.circe" %%% "circe-core" % CirceVersion,
        "io.circe" %%% "circe-generic" % CirceVersion,
        "io.circe" %%% "circe-parser" % CirceVersion,
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

lazy val klib =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib"))
    .settings(
      name := "klib",
      version := "1.2.16",
      Compile / unmanagedSourceDirectories +=
        baseDirectory.value / "shared" / "main" / "scala",
      Test / unmanagedSourceDirectories +=
        baseDirectory.value / "shared" / "test" / "scala",
      scalaVersion := Scala_3,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      scalacOptions += "-source:future",
      // Dependencies
      Dependencies.`dev.zio`.zio,
      Dependencies.`dev.zio`.`zio-streams`,
      Dependencies.`dev.zio`.`zio-test`,
      Dependencies.`dev.zio`.`zio-test-sbt`,
      Dependencies.`io.circe`.`circe-core`,
      Dependencies.`io.circe`.`circe-generic`,
      Dependencies.`io.circe`.`circe-parser`,
      Dependencies.`org.typelevel`.`cats-effect`,
      Dependencies.`com.github.julien-truffaut`.`monocle-core`,
      Dependencies.`com.github.julien-truffaut`.`monocle-macro`,
      // Testing
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    )
    .jsSettings(
      Dependencies.`io.github.cquiroz`.`scala-java-time`,
      Dependencies.`com.lihaoyi`.scalatags,
      // TODO: Remove
      scalaJSUseMainModuleInitializer := true,
    )
    .jvmSettings(
      // Dependencies
      Dependencies.`com.google.jimfs`.jimfs,
      Test / run / fork := true,
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
      klib.js,
      klib.jvm,
    )
