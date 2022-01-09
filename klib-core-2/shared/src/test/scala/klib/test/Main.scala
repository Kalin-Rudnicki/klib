package klib.test

import zio._
import zio.Console

object Main extends ZIOAppDefault {

  private def makeNum(i: Int): Int = {
    println(s"Making number: $i")
    i
  }

  extension[A](a: => A) def wrapped: UIO[A] = ZIO.succeed(a)

  override def run: RIO[Environment, Any] =
    for {
      _ <- Console.printLine("[1]")
      myNumZIO = makeNum(5).wrapped
      _ <- Console.printLine("[2]")
      myNum <- myNumZIO
      _ <- Console.printLine(s"my-num: $myNum")
    } yield ()

}
