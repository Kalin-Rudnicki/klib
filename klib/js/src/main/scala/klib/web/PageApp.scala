package klib.web

import zio.*

import klib.utils.*
import klib.utils.commandLine.parse.*

trait PageApp extends ExecutableApp {

  // TODO: Load from url?
  private val args: TaskM[Chunk[String]] =
    ZIO.succeed(Chunk("-l=DEBUG", "-r=DEV", "--"))

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    super.run.provideSomeLayer(
      args.map(ZIOAppArgs(_)).toLayer,
    )

  val routeMatcher: RouteMatcher[Page]

  override val executable: Executable =
    Executable
      .fromParser(Parser.unit.disallowExtras)
      .withLayer(_ => ZIO.unit.toLayer)
      .withExecute { _ =>
        for {
          renderer <- VDomActions.Renderer.Initial
          runtime <- ZIO.runtime[Executable.BaseEnv]

          _ <- routeMatcher.attemptToLoadPage(renderer, runtime)
        } yield ()
      }

}
