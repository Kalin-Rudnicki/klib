package klib.fp.typeclass

trait Functor[T[_]] {

  def map[A, B](t: T[A], f: A => B): T[B]

}

object Functor {

  trait Implicits {

    implicit class FunctorOps[T[_]: Functor, A](t: T[A]) {

      private val functor: Functor[T] = implicitly[Functor[T]]

      def map[B](f: A => B): T[B] =
        functor.map(t, f)

    }

  }
  object Implicits extends Implicits

  trait Instances {

    // TODO (KR) :

  }
  object Instances extends Instances

}
