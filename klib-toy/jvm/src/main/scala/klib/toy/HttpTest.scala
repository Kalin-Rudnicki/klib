package klib.toy

import zhttp.http.*
import zhttp.service.Server
import zio.*

import klib.utils.*
import klib.utils.commandLine.parse.*

object HttpTest extends ExecutableApp {

  val indexApp =
    Http.collect[Request] { case Method.GET -> !! => Response.text("INDEX!") }

  val randomApp =
    Http.collectZIO[Request] {
      case Method.GET -> !! / "int"    => Random.nextInt.map(i => Response.text(i.toString))
      case Method.GET -> !! / "double" => Random.nextDouble.map(i => Response.text(i.toString))
    }

  val httpApp =
    Http.collectHttp[Request] {
      case _ -> !! / "random" => randomApp
      case req                => Response.apply()
    }

  override val executable: Executable =
    Executable
      .fromParser(Parser.unit.disallowExtras)
      .withLayer { _ => ZLayer.succeed(()) }
      .withExecute { _ =>
        Server.start(2451, httpApp).mapError(KError.SystemFailure("Error with HTTP server", _)).toErrorNEL
      }

}
