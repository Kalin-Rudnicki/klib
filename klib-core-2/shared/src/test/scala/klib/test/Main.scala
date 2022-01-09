package klib.test

import cats.syntax.option._
import zio._
import zio.Console

import klib.utils._

object Main extends ZIOAppDefault {

  override def run: RIO[Environment, Any] =
    for {
      logger <- Logger(
        flags = Set.empty,
        flagMap = Map.empty,
        defaultIndent = "    ",
        colorMode = Logger.ColorMode.Extended,
        sources = List(Logger.Source.stdOut(Logger.LogLevel.Info)),
        initialIndents = Nil,
      )
      events = Logger.Event.Compound(
        (None :: Logger.LogLevel.All.map(_.some)).map { logLevel =>
          def event: Logger.Event = Logger.Event.Println(logLevel.fold("(None)")(_.displayName))

          logLevel match {
            case Some(logLevel) => Logger.Event.RequireLogLevel(logLevel, () => event)
            case None           => event
          }
        },
      )
      (duration, _) <- logger
        .execute(events)
        .timed
      _ <- logger.execute(
        Logger.Event.Println(duration.toNanos),
      )
    } yield ()

}
