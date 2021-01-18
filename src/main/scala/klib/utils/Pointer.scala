package klib.utils

import klib.Implicits._

final class Pointer[T] private {
  private var set: Boolean = false
  private var value: T = _
}

object Pointer {

  def apply[T](t: T): Pointer[T] = {
    val ptr = new Pointer[T]
    ptr.value = t
    ptr.set = true
    ptr
  }

  def unapply[T](arg: Pointer[T]): Option[T] =
    arg.set.maybe(arg.value).toOption

  def withSelf[T](f: Pointer[T] => Pointer[T]): Pointer[T] = {
    val self = new Pointer[T]
    val res = f(self)
    self.value = res.value
    self.set = true
    self
  }

}
