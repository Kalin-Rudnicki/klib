package klib.fp.types

import klib.Implicits._
import klib.fp.typeclass._

import scala.annotation.unchecked.{uncheckedVariance => uV}

final class WrappedErrorAccumulator[+T[+_], +E, +W, +R] private (private val wrapped: T[ErrorAccumulator[E, W, R]]) {

  def run(implicit runSync: RunSync[T @uV, E @uV]): ErrorAccumulator[E, W, R] =
    runSync.runSync(wrapped) match {
      case Right(b) =>
        b
      case Left(a) =>
        Dead(a :: Nil)
    }

}

object WrappedErrorAccumulator {

  type Projection[T2[+_], E, W] = { type T[R] = WrappedErrorAccumulator[T2, E, W, R] }

  implicit def wrappedErrorAccumulatorMonad[T2[+_], E, W](implicit
      tMonad: Monad[T2],
      tRunSync: RunSync[T2, E],
      eaMonad: Monad[ErrorAccumulator.Projection[E, W]#T],
  ): Monad[Projection[T2, E, W]#T] =
    new Monad[Projection[T2, E, W]#T] {

      override def map[A, B](
          t: WrappedErrorAccumulator[T2, E, W, A],
          f: A => B,
      ): WrappedErrorAccumulator[T2, E, W, B] =
        new WrappedErrorAccumulator[T2, E, W, B](
          tMonad.map(
            t.wrapped,
            (a: ErrorAccumulator[E, W, A]) => {
              eaMonad.map(a, f)
            },
          ),
        )

      override def apply[A, B](
          t: WrappedErrorAccumulator[T2, E, W, A],
          f: WrappedErrorAccumulator[T2, E, W, A => B],
      ): WrappedErrorAccumulator[T2, E, W, B] =
        new WrappedErrorAccumulator(
          tMonad.pure {
            (tRunSync.runSync(t.wrapped), tRunSync.runSync(f.wrapped)) match {
              case (Right(t), Right(f)) =>
                eaMonad.apply(t, f)
              case (Right(t), Left(f)) =>
                eaMonad.apply(t, Dead(f :: Nil))
              case (Left(t), Right(f)) =>
                eaMonad.apply(Dead(t :: Nil), f)
              case (Left(t), Left(f)) =>
                eaMonad.apply(Dead(t :: Nil), Dead(f :: Nil))
            }
          },
        )

      override def pure[A](a: => A): WrappedErrorAccumulator[T2, E, W, A] =
        new WrappedErrorAccumulator(tMonad.pure(eaMonad.pure(a)))

      override def flatten[A](
          t: WrappedErrorAccumulator[T2, E, W, WrappedErrorAccumulator[T2, E, W, A]],
      ): WrappedErrorAccumulator[T2, E, W, A] =
        new WrappedErrorAccumulator(
          tMonad.map(
            t.wrapped,
            (a1: ErrorAccumulator[E, W, WrappedErrorAccumulator[T2, E, W, A]]) => {
              eaMonad.flatMap(
                a1,
                (a2: WrappedErrorAccumulator[T2, E, W, A]) => {
                  tRunSync.runSync(a2.wrapped) match {
                    case Right(r) =>
                      r
                    case Left(a) =>
                      Dead(a :: Nil)
                  }
                },
              )
            },
          ),
        )

    }

}
