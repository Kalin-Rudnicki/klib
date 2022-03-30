package klib.web.widgets

import cats.Monoid
import cats.syntax.monoid.*

import klib.utils.*
import klib.web.*

type AVSubmit[T, O] = AVWidget[Submit, T, O]

trait NameFunction[+T] { self =>
  def apply(rawName: String, prettyName: String): T
}
object NameFunction {
  def const[T](t: => T): NameFunction[T] = (_, _) => t
}

extension [T: Monoid](self: NameFunction[T]) {
  def |>|(other: NameFunction[T]): NameFunction[T] = (rn, pn) => self(rn, pn) |+| other(rn, pn)
  def |<|(other: NameFunction[T]): NameFunction[T] = (rn, pn) => other(rn, pn) |+| self(rn, pn)
}
