package klib.test

import cats.syntax.option._
import zio._
import zio.Console

import klib.utils._

object Main extends ZIOApp {

  override type Environment = ZEnv with Logger

  override def layer: ZLayer[ZIOAppArgs, Any, Environment] =
    ZEnv.live ++
      ZLayer.fromZIO(
        Logger(
          flags = Set.empty,
          flagMap = Map.empty,
          defaultIndent = "|   ",
          colorMode = Logger.ColorMode.Extended,
          sources = List(Logger.Source.stdOut(Logger.LogLevel.Info)),
          initialIndents = Nil,
        ),
      )

  override implicit def tag: Tag[Environment] = Tag[Environment]

  override def run: RIO[Environment, Any] =
    for {
      _ <- Logger.println("Test")
      _ <- Logger
        .withIndent(1) {
          for {
            _ <- Logger.break()
            _ <- Logger.println.info("Test2")
            _ <- Logger.println.debug("Test3")
            _ <- ZIO.fail(())
          } yield ()
        }
        .orElse(ZIO.succeed(()))
      _ <- Logger.break()
      _ <- Logger.println.warning("Test4", "Test5")
      _ <- Logger.break()
      _ <- Logger.break()
      _ <- Logger.println.error("Test6")
      _ <- Logger.break.open()
      _ <- Logger.break.close()
      _ <- Logger.println.error("Test7")
    } yield ()

}
