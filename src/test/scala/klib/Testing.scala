package klib

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado

object Testing extends App {

  implicit class IOOps[T](t: => T) {

    def toIo: IO[T] =
      IO(t)

  }

  def makeIo(i: Int): IO[Int] =
    IO {
      println(s"[$i]")
      i
    }

  val io1 = makeIo(1)
  val io2 = makeIo(2)
  val io3 = makeIo(3)

  val ioSum =
    ado[IO].join(io1, io2, io3)

  ioSum.runSync

}
