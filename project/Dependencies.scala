// ...

import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  // format: off
  sealed abstract class Org(org: String) {
    protected def rawDep(projectName: String, version: String, f: ModuleID => ModuleID = identity): Def.SettingsDefinition = libraryDependencies += f(org % projectName % version)
    protected def dep(projectName: String, version: String, f: ModuleID => ModuleID = identity): Def.SettingsDefinition = libraryDependencies += f(org %% projectName % version)
    protected def crossDep(projectName: String, version: String, f: ModuleID => ModuleID = identity): Def.SettingsDefinition = libraryDependencies += f(org %%% projectName % version)
  }
  // format: on

  // =====| Dependencies |=====

  object `com.google.jimfs` extends Org("com.google.jimfs") {
    val jimfs = rawDep("jimfs", "1.2")
  }

  object `io.circe` extends Org("io.circe") {
    private val version = "0.15.0-M1"

    val `circe-core` = crossDep("circe-core", version)
    val `circe-generic` = crossDep("circe-generic", version)
    val `circe-parser` = crossDep("circe-parser", version)
  }

  object `dev.zio` extends Org("dev.zio") {
    private val version = "2.0.0-RC1"

    val zio = crossDep("zio", version)
    val `zio-test` = crossDep("zio-test", version, _ % Test)
  }

  object `org.typelevel` extends Org("org.typelevel") {
    val `cats-effect` = crossDep("cats-effect", "3.3.3")
  }

  object `org.rogach` extends Org("org.rogach") {
    val scallop = dep("scallop", "4.0.4")
  }

}
