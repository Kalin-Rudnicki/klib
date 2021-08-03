package klib.fp.typeclass

trait Applicative[T[_]] extends Functor[T] {

  def apply[A, B](t: T[A], f: T[A => B]): T[B]

  def pure[A](a: => A): T[A]

  final def aToF[A, B, C](t: T[A], f: (A, B) => C): T[B => C] =
    map[A, B => C](t, t => f(t, _))

  final def aJoin[A, B](t: T[A], t2: T[B]): T[(A, B)] =
    apply[A, (A, B)](
      t,
      aToF[B, A, (A, B)](
        t2,
        (a, b) => (b, a),
      ),
    )

}

object Applicative {

  trait Implicits {

    implicit class ApplicativeOps[T[_]: Applicative, A](t: T[A]) { // extends Functor.Implicits.FunctorOps(t) {

      private val applicative: Applicative[T] = implicitly[Applicative[T]]

      def apply[B](f: T[A => B]): T[B] =
        applicative.apply(t, f)

      def aToF[B, C](f: (A, B) => C): T[B => C] =
        applicative.aToF(t, f)

      def aJoin[B](t2: T[B]): T[(A, B)] =
        applicative.aJoin(t, t2)

    }

    implicit class ApplicativeLiftOps[A](a: => A) {

      def pure[T[_]](implicit applicative: Applicative[T]): T[A] =
        applicative.pure(a)

    }

  }
  object Implicits extends Implicits

  trait Instances {

    // TODO (KR) :

  }
  object Instances extends Instances

}
