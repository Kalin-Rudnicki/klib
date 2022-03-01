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
        for {
          f1 <- File.fromPath("a/b/c")
          f2 <- File.fromPath("a/b/c.ext")
          f3 <- File.fromPath("a/b/c.ext.another_ext")
          _ <- Logger.println.info(f1.fileName)
          _ <- Logger.println.info(f2.fileName)
          _ <- Logger.println.info(f3.fileName)
        } yield ()
      }

}
