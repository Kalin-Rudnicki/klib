package klib.fp.typeclass

trait Monad[T[_]] extends Applicative[T] {

  def flatten[A](t: T[T[A]]): T[A]

  def flatMap[A, B](t: T[A], f: A => T[B]): T[B] =
    flatten(map(t, f))

  def flatApply[A, B](t: T[A], f: T[A => T[B]]): T[B] =
    flatten(apply(t, f))

}

object Monad {

  trait Implicits {

    implicit class MonadOps[T[_]: Monad, A](t: T[A]) { // extends Applicative.Implicits.ApplicativeOps(t) {

      private val monad: Monad[T] = implicitly[Monad[T]]

      def flatMap[B](f: A => T[B]): T[B] =
        monad.flatMap(t, f)

    }

    implicit class NestedMonadOps[T[_]: Monad, A](t: T[T[A]]) {

      private val monad: Monad[T] = implicitly[Monad[T]]

      def flatten: T[A] = monad.flatten(t)

    }

  }
  object Implicits extends Implicits

  trait Instances {

    // TODO (KR) :

  }
  object Instances extends Instances

}
