package klib.fp.typeclass

trait Functor[T[_]] {

  extension[A](t: T[A]) def map[B](f: A => B): T[B]

}
