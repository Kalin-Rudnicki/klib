package klib.utils

final class Lazy[+T](t: => T) {

  lazy val value: T = t

  override def toString: String =
    s"Lazy($value)"

}

object Lazy {

  def apply[T](t: => T): Lazy[T] =
    new Lazy(t)

}
