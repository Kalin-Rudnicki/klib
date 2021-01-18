//

lazy val klib =
  project
    .in(file("."))
    .settings(
      name := "klib",
      version := "0.0.1",
      scalaVersion := "2.12.10",
      libraryDependencies ++=
        Seq() ++
          Seq(
            "org.scalatest" %% "scalatest" % "3.2.3",
          ).map(_ % Test),
    )
