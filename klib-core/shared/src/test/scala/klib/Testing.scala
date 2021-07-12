package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  {
    for {
      a <- 5.pure[IO]
      b <- 0.pure[IO]
      c = a / b
      _ <- println(c).pure[IO]
    } yield c
  }.runSyncOrDump(Logger(Logger.LogLevel.Info).some)

}
