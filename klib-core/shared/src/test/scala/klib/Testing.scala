package klib

import klib.Implicits._
import klib.fp.types._

object Testing extends App {

  def stuff(i: Maybe[Int]): ?[Any] =
    for {
      i2 <- i.pure[?].toMaybeMonad
    } yield (i2)

  println {
    stuff(5.some)
  }

  println {
    stuff(None)
  }

}
