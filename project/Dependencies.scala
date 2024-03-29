// ...

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._
import sbt.Keys._

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

  object `dev.zio` extends Org("dev.zio") {
    private val version = "2.0.0-RC6"

    val zio = crossDep("zio", version)
    val `zio-streams` = crossDep("zio-streams", version)
    val `zio-json` = crossDep("zio-json", "0.3.0-RC8")

    val `zio-test` = crossDep("zio-test", version, _ % Test)
    val `zio-test-sbt` = crossDep("zio-test-sbt", version, _ % Test)
  }

  object `io.getquill` extends Org("io.getquill") {
    val `quill-jdbc-zio` = dep("quill-jdbc-zio", "4.0.0-RC1")
  }

  object `io.d11` extends Org("io.d11") {
    val zhttp = dep("zhttp", "2.0.0-RC9")
  }

  object `org.typelevel` extends Org("org.typelevel") {
    val `cats-effect` = crossDep("cats-effect", "3.3.3")
  }

  object `io.github.cquiroz` extends Org("io.github.cquiroz") {
    val `scala-java-time` = crossDep("scala-java-time", "2.3.0")
  }

  object `com.lihaoyi` extends Org("com.lihaoyi") {
    val scalatags = crossDep("scalatags", "0.11.1")
  }

  object `com.github.julien-truffaut` extends Org("com.github.julien-truffaut") {
    private val version = "3.0.0-M6"

    val `monocle-core` = crossDep("monocle-core", version)
    val `monocle-macro` = crossDep("monocle-macro", version)
  }

}
