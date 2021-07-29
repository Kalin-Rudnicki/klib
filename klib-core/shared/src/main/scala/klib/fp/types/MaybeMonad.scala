package klib.fp.types

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.fp.utils._

final class MaybeMonad[T[_], A](val wrapped: T[Maybe[A]])
object MaybeMonad {

  type Projection[T[_]] = { type P[A] = MaybeMonad[T, A] }

  implicit def maybeMonadMonad[T[_]](implicit tMonad: Monad[T]): Monad[Projection[T]#P] =
    new Monad[Projection[T]#P] {

      override def map[A, B](t: MaybeMonad[T, A], f: A => B): MaybeMonad[T, B] =
        new MaybeMonad(t.wrapped.map(_.map(f)))

      override def apply[A, B](t: MaybeMonad[T, A], f: MaybeMonad[T, A => B]): MaybeMonad[T, B] =
        new MaybeMonad(
          ado[T]
            .join(
              t.wrapped,
              f.wrapped,
            )
            .map {
              case (t, f) =>
                t.apply(f)
            },
        )

      override def pure[A](a: => A): MaybeMonad[T, A] =
        new MaybeMonad(a.some.pure[T])

      override def flatten[A](t: MaybeMonad[T, MaybeMonad[T, A]]): MaybeMonad[T, A] =
        new MaybeMonad(
          t.wrapped
            .flatMap {
              case Some(inner) =>
                inner.wrapped
              case None =>
                Maybe
                  .empty[A]
                  .pure[T]
            },
        )

    }

}
