package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  val logger: Logger = Logger(Logger.LogLevel.Debug)

  (
    for {
      r1 <- (
          for {
            _ <- 1.pure[??]
            _ <- 2.pure[??]
            _ <- 3.pure[??]
            _ <-
              (
                if (false)
                  4.pure[?]
                else
                  Dead(Message("Oops") :: Nil)
              ).wrap[IO]
          } yield ()
      ).run.pure[??]
      _ <- r1 match {
        case Alive(_, _) =>
          logger() { src =>
            src.info("Lived...")
          }.wrap
        case Dead(errors, _) =>
          logger() { src =>
            errors.foreach(src.logThrowable(_))
          }.wrap
      }
    } yield ()
  ).run

}
