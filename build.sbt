//

lazy val klib =
  project
    .in(file("."))
    .settings(
      name := "klib",
      organization := "kalin-rudnicki",
      version := "0.0.2",
      scalaVersion := "2.13.4",
      crossScalaVersions := Seq("2.12.10", "2.13.4"),
      libraryDependencies ++=
        Seq() ++
          Seq(
            "org.scalatest" %% "scalatest" % "3.2.3",
          ).map(_ % Test),
      resolvers += Resolver.mavenLocal,
    )
