package klib

import scala.concurrent._
import scala.concurrent.duration.Duration

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  val io1: IO[Unit] = IO { Thread.sleep(2500) }
  val asyncIO: AsyncIO[Unit] = io1.toAsyncIO
  val io2: IO[Unit] = asyncIO.toIOGlobal(Duration(3, "s").some)

  println(io2.runSync)

}
