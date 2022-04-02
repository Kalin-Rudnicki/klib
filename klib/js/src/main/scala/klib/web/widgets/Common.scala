package klib.web.widgets

import cats.Monoid
import cats.syntax.monoid.*

import klib.web.*
import klib.web.widgets.CommonRaises.*

type AVSubmit[T, O] = AVWidget[Submit, T, O]

trait NameFunction[+T] { self =>
  def apply(rawName: String, prettyName: String): T
}
object NameFunction {
  def const[T](t: => T): NameFunction[T] = (_, _) => t

  given [T](using Monoid[T]): Monoid[NameFunction[T]] =
    new Monoid[NameFunction[T]] {
      def empty: NameFunction[T] = NameFunction.const(Monoid[T].empty)
      def combine(x: NameFunction[T], y: NameFunction[T]): NameFunction[T] =
        (rawName, prettyName) => x(rawName, prettyName) |+| y(rawName, prettyName)
    }

}

extension [T: Monoid](self: NameFunction[T]) {
  def |>|(other: NameFunction[T]): NameFunction[T] = self |+| other
  def |<|(other: NameFunction[T]): NameFunction[T] = other |+| self
}
