package klib.test

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import javax.swing.*
import zio.*

import klib.utils.*
import klib.utils.commandLine.parse.*

object JvmMain extends ExecutableApp {

  override val executable: Executable =
    Executable
      .fromParser(Parser.unit.disallowExtras)
      .withLayer(_ => ZLayer.succeed(()))
      .withExecute { _ =>
        ZIO.unit
      }

}
