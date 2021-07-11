package klib.utils

import klib.extensions.{given, _}
import klib.fp.typeclass._

final class Pointer[T] private {

  private var _set: Boolean = false
  private var _value: T = _

  def value: T =
    this match { case Pointer(value) => value }

  override def equals(obj: Any): Boolean =
    obj match {
      case that: Pointer[T] =>
        this._set && that._set && this._value == that._value
      case _ =>
        false
    }

}

object Pointer {

  def apply[T](t: T): Pointer[T] = {
    val ptr = new Pointer[T]
    ptr._value = t
    ptr._set = true
    ptr
  }

  def unapply[T](arg: Pointer[T]): Option[T] =
    arg._set.maybe(arg._value).toOption

  def withSelf[T](f: Pointer[T] => Pointer[T]): Pointer[T] = {
    val self = new Pointer[T]
    val res = f(self)
    self._value = res._value
    self._set = true
    self
  }

  def withSelfWrapped[T, F[_]: Functor](f: Pointer[T] => F[Pointer[T]]): F[Pointer[T]] = {
    val self = new Pointer[T]
    val res = f(self)
    res.map { t =>
      self._value = t._value
      self._set = true
      self
    }
  }

}
