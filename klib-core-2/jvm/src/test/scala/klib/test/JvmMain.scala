package klib.test

import cats.syntax.option.*
import zio.*
import zio.Console

import klib.utils.*

object JvmMain extends ZIOApp {

  override type Environment = ZEnv with Logger

  override def layer: ZLayer[ZIOAppArgs, Any, Environment] =
    ZEnv.live ++ Logger.live(Logger.LogLevel.Detailed)

  override implicit def tag: Tag[Environment] = Tag[Environment]

  override def run: RIO[Environment, Any] =
    for {
      _ <- Console.printLine("=====| JVM Main |=====")
    } yield ()

}
