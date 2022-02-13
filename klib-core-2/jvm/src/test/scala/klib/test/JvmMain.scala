package klib.test

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import zio.*

import klib.utils.*
import klib.utils.commandLine.parse.*

object JvmMain extends ExecutableApp {

  private def intToEither(int: Int): Either[String, Int] =
    Either.cond(int != 0, int, "int == 0")

  private def showIntTraverse(int: Option[Int]): URIO[Logger, Unit] =
    Logger.println.info(int.traverse(intToEither))

  override val executable: Executable =
    Executable
      .fromParser(Parser.unit.disallowExtras)
      .withLayer(_ => ZLayer.succeed(()))
      .withExecute { _ =>
        for {
          _ <- showIntTraverse(None)
          _ <- showIntTraverse(5.some)
          _ <- showIntTraverse(0.some)
        } yield ()
      }

}
