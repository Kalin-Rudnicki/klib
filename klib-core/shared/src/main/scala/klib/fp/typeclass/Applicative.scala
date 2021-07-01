package klib.fp.typeclass

trait Applicative[T[_]] extends Functor[T] {

  extension [A](t: T[A]) {

    def apply[B](f: T[A => B]): T[B]

    def aToF[B, C](f: (A, B) => C): T[B => C] =
      t.map(t => f(t, _))

    def aJoin[B](t2: T[B]): T[(A, B)] =
      t.apply(t2.aToF((a, b) => (b, a)))

  }

  def pure[I](i: => I): T[I]

}

object Applicative {

  object extensions {

    implicit class ApplicativeIdOpts[I](i: => I) {

      def pure[T[_]: Applicative]: T[I] = summon[Applicative[T]].pure(i)

    }

  }

}
