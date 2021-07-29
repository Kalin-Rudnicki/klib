package klib.fp

import scala.util.Try

import klib.fp.typeclass.Implicits._
import klib.fp.typeclass._
import klib.fp.utils.ado
import klib.utils.@@

package object types {

  type \/[+A, +B] = Either[A, B]

  type ?[+R] = ErrorAccumulator[Throwable, R]
  object ? {
    def apply[R](r: => R): ?[R] = r.pure[?]

    def dead(throwables: Throwable*): ?[Nothing] =
      Dead(throwables.toList)
  }
  final case class Message(message: String, cause: Maybe[Throwable] = None)
      extends Throwable(message, cause.getOrElse(null)) {

    override def toString: String =
      s"Message($message${cause.cata(c => s", cause: $c", "")})"

  }

  final case class Compound(children: List[Throwable]) extends Throwable(children.map(_.getMessage).mkString("\n"))

  trait Implicits extends Maybe.Implicits with Either.Implicits with ErrorAccumulator.Implicits with NonEmptyList.Implicits {

    implicit class OptionIdOps[A](a: A) {

      def someOpt: Option[A] =
        scala.Some(a)

    }

    implicit class OptionOps[A](a: scala.Option[A]) {

      def toMaybe: Maybe[A] =
        a match {
          case scala.Some(value) =>
            Some(value)
          case scala.None =>
            None
        }

    }

    implicit class EitherOps[A, B](a: scala.Either[A, B]) {

      def toErrorAccumulator: ErrorAccumulator[A, B] =
        a match {
          case scala.Right(value) =>
            value.alive
          case scala.Left(error) =>
            Dead(error :: Nil)
        }

      def to_\/ : Either[A, B] =
        a match {
          case scala.Left(value) =>
            Left(value)
          case scala.Right(value) =>
            Right(value)
        }

    }

    implicit class TryOps[A](a: Try[A]) {

      def to_? : ?[A] =
        a.toEither.toErrorAccumulator

      def to_\/ : Either[Throwable, A] =
        a.toEither.to_\/

    }

    implicit class BooleanOps(b: Boolean) {

      def maybe[A](a: => A): Maybe[A] =
        if (b)
          Some(a)
        else
          None

      final class AwaitingIfFalse[+A](ifTrue: => A) {

        def |[A2 >: A](ifFalse: => A2): A2 =
          if (b)
            ifTrue
          else
            ifFalse

      }

      def ?[A](ifTrue: => A): AwaitingIfFalse[A] =
        new AwaitingIfFalse(ifTrue)

    }

    implicit class MaybeMonadOps[T[_], A](t: T[Maybe[A]]) {
      import klib.utils.Implicits._

      def toMaybeMonad: MaybeMonad[T, A] =
        t.wrap[MaybeMonad[T, A]]

    }

    implicit def maybeMonadMonad[T[_]](implicit tMonad: Monad[T]): Monad[MaybeMonad.Projection[T]#P] =
      new Monad[MaybeMonad.Projection[T]#P] {
        import klib.utils.Implicits._

        override def map[A, B](t: MaybeMonad[T, A], f: A => B): MaybeMonad[T, B] =
          t.unwrap
            .map(_.map(f))
            .wrap[MaybeMonad[T, B]]

        override def apply[A, B](t: MaybeMonad[T, A], f: MaybeMonad[T, A => B]): MaybeMonad[T, B] =
          ado[T]
            .join(
              t.unwrap,
              f.unwrap,
            )
            .map {
              case (t, f) =>
                t.apply(f)
            }
            .wrap[MaybeMonad[T, B]]

        override def pure[A](a: => A): MaybeMonad[T, A] =
          (Some(a): Maybe[A])
            .pure[T]
            .wrap[MaybeMonad[T, A]]

        override def flatten[A](t: MaybeMonad[T, MaybeMonad[T, A]]): MaybeMonad[T, A] =
          t.unwrap
            .flatMap {
              case Some(inner) =>
                inner.unwrap
              case None =>
                (None: Maybe[A]).pure[T]
            }
            .wrap[MaybeMonad[T, A]]

      }

  }
  object Implicits extends Implicits

  sealed trait MaybeMonadT
  type MaybeMonad[T[_], A] = T[Maybe[A]] @@ MaybeMonadT
  object MaybeMonad {
    type Projection[T[_]] = { type P[A] = MaybeMonad[T, A] }
  }

}
