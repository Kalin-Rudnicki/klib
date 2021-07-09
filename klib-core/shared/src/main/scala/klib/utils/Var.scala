package klib.utils

import scala.annotation.unchecked.uncheckedVariance

import klib.fp.types._

final class Var[+T](initialValue: T) {
  private var _value: T @uncheckedVariance = initialValue

  def value: T = _value
  def value_=(newValue: T @uncheckedVariance): IO[Unit] = IO(this._value = newValue)

  override def toString: String =
    s"Var(${Maybe(_value).cata(_.toString, "null")})"

}
object Var {

  def apply[T](initialValue: T): Var[T] =
    new Var(initialValue)

  def `null`[T]: Var[T] =
    Var(null.asInstanceOf[T])

}
