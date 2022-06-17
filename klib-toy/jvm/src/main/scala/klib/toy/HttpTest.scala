package klib.toy

import zhttp.http.*
import zhttp.service.Server
import zio.*

import klib.utils.*
import klib.utils.commandLine.parse.*

object HttpTest extends ExecutableApp {

  override val executable: Executable =
    Executable
      .fromParser(Parser.unit.disallowExtras)
      .withLayer { _ => ZLayer.succeed(()) }
      .withExecute { _ =>
        Logger.println.info("Hello world!")
      }

}
