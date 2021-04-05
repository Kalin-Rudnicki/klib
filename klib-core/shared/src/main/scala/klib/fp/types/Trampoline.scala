package klib.fp.types

import scala.annotation.tailrec

sealed trait Trampoline[+T] {

  def result: T = {
    @tailrec
    def loop(t: Trampoline[T]): T =
      t match {
        case Trampoline.More(f) => loop(f())
        case Trampoline.Done(r) => r
      }

    loop(this)
  }

}

object Trampoline {

  final case class More[+T](f: () => Trampoline[T]) extends Trampoline[T]

  final case class Done[+T](r: T) extends Trampoline[T]

}
