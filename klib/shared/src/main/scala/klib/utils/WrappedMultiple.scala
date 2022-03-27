package klib.utils

import cats.data.NonEmptyList
import cats.syntax.either.*

enum WrappedMultiple[+W] {

  case Single(w: W)
  case Multiple(single0: W, single1: W, singleN: List[W])

  def toNEL: NonEmptyList[W] =
    this match {
      case Single(w)            => NonEmptyList.one(w)
      case Multiple(w0, w1, wN) => w0 :: NonEmptyList(w1, wN.toList)
    }

  def toEither: Either[W, NonEmptyList[W]] =
    this match {
      case Single(w)            => w.asLeft
      case Multiple(w0, w1, wN) => NonEmptyList(w0, w1 :: wN.toList).asRight
    }

}
object WrappedMultiple {

  def apply[W](w0: W, wN: W*): WrappedMultiple[W] =
    wN.toList match {
      case Nil      => Single(w0)
      case w1 :: wN => Multiple(w0, w1, wN)
    }

  def flatten[W](w0: WrappedMultiple[W], wN: WrappedMultiple[W]*): WrappedMultiple[W] =
    NonEmptyList(w0, wN.toList).flatMap(_.toNEL) match {
      case NonEmptyList(w, Nil)       => Single(w)
      case NonEmptyList(w0, w1 :: wN) => Multiple(w0, w1, wN)
    }

}
