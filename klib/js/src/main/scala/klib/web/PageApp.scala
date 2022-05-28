package klib.web

import zio.*

import klib.utils.*

trait PageApp extends ZIOAppDefault {

  val routeMatcher: RouteMatcher[Page]

  private final type PageEnv = Executable.Env

  private final val pageLayer: KRLayer[ZIOAppArgs & ZEnv, Executable.Env] =
    FileSystem.jsUnimplemented ++
      Logger.live(Logger.LogLevel.Debug) ++
      ZLayer.succeed(RunMode.Dev)

  private final val prog: SKTask[Unit] =
    for {
      renderer <- VDomActions.Renderer.Initial
      runtime <- ZIO.runtime[Executable.BaseEnv]

      _ <- routeMatcher.attemptToLoadPage(renderer, runtime)
    } yield ()

  override def run: KRIO[ZIOAppArgs & ZEnv, Unit] =
    prog.provideSomeLayer(pageLayer)

}
