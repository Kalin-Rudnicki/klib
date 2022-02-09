package klib.test

import cats.syntax.option.*
import zio.*

import klib.utils.*

object SharedMain extends ZIOApp {

  override type Environment = ZEnv with Logger

  override def layer: ZLayer[ZIOAppArgs, Any, Environment] =
    ZEnv.live ++
      ZLayer.fromZIO(
        Logger(
          flags = Set.empty,
          flagMap = Map.empty,
          defaultIndent = "|   ",
          colorMode = ColorMode.Extended,
          sources = List(Logger.Source.stdOut(Logger.LogLevel.Info)),
          initialIndents = Nil,
        ),
      )

  override implicit def tag: Tag[Environment] = Tag[Environment]

  override def run: RIO[Environment, Any] =
    for {
      _ <- Logger.println("=====| Shared Main |=====")
    } yield ()

}
