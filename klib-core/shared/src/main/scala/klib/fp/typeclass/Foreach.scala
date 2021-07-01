package klib.fp.typeclass

trait Foreach[T[_]] {

  extension[A](t: T[A]) def foreach(f: A => Unit): Unit

}
