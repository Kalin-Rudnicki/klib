package klib.test

import cats.data.NonEmptyList
import cats.syntax.option.*
import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import zio.*

import klib.utils.*
import klib.utils.commandLine.parse.*

object JvmMain extends ExecutableApp {

  override val executable: Executable =
    Executable
      .fromParser(Parser.singleValue[Int]("int").required.disallowExtras)
      .withLayer(_ => ZLayer.succeed(()))
      .withExecute { _ =>
        for {
          _ <- Logger.println.info("Hello World!")
          _ <- ZIO.fail(NonEmptyList.one(Message.withUserMessage("dev", "user")))
        } yield ()
      }

}
