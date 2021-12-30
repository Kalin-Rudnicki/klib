package klib.fp

import scala.util.Try

import klib.fp.typeclass.Implicits._
import klib.fp.typeclass._

package object types {

  type \/[+A, +B] = Either[A, B]

  type ?[+R] = ErrorAccumulator[Throwable, R]
  object ? {

    def apply[R](r: => R): ?[R] = r.pure[?]

    def dead(throwables: Throwable*): ?[Nothing] =
      Dead(throwables.toList)

    def error(throwables: Throwable*): ?[Nothing] =
      Dead(throwables.toList)

    def errorMessage(messages: String*): ?[Nothing] =
      Dead(messages.toList.map(Message(_)))

    def ??? : ?[Nothing] =
      error(Message("??? (Unimplemented)"))

  }
  final case class Message(message: String, cause: Maybe[Throwable] = None)
      extends Throwable(message, cause.getOrElse(null)) {

    override def toString: String =
      s"Message($message${cause.cata(c => s", cause: $c", "")})"

  }

  type Identity[T] = T
  object Identity {
    def apply[T](t: T): Identity[T] = t
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

      def toMaybeMonad: MaybeMonad[T, A] =
        new MaybeMonad(t)

    }

    // ---  ---

    implicit val identityMonad: Monad[Identity] =
      new Monad[Identity] {

        override def map[A, B](t: Identity[A], f: A => B): Identity[B] = f(t)

        override def apply[A, B](t: Identity[A], f: Identity[A => B]): Identity[B] = f(t)

        override def pure[A](a: => A): Identity[A] = a

        override def flatMap[A, B](t: Identity[A], f: A => Identity[B]): Identity[B] = f(t)

      }

    implicit val identityTraverseList: Traverse[List, Identity] =
      new Traverse[List, Identity] {

        override def traverse[T](t: List[Identity[T]]): Identity[List[T]] = t

      }

    implicit def anythingTraverseIdentity[A[_]]: Traverse[Identity, A] =
      new Traverse[Identity, A] {

        override def traverse[T](t: Identity[A[T]]): A[Identity[T]] = t

      }

  }
  object Implicits extends Implicits

}
