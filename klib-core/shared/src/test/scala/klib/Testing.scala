package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  def stuff(i: Maybe[Int]): ?[Any] =
    for {
      i1 <- i.pure[?]
      i2 <- i1.pure[?].toMaybeMonad
    } yield (i1, i2)

  println {
    stuff(5.some)
  }

  println {
    stuff(None)
  }

}
