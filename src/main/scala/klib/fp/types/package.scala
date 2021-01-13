package klib.fp

import scala.util.Try

package object types {

  type \/[+A, +B] = Either[A, B]

  type ?[+R] = ErrorAccumulator[Throwable, Throwable, R]

  final case class Message(message: String) extends Throwable(message) {

    override def toString: String =
      s"Message($message)"

  }

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

      def toErrorAccumulator: ErrorAccumulator[A, Nothing, B] =
        a match {
          case scala.Right(value) =>
            value.alive
          case scala.Left(error) =>
            Dead(error :: Nil)
        }

    }

    implicit class TryOps[A](a: Try[A]) {

      def to_? : ?[A] =
        a.toEither.toErrorAccumulator

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

  }
  object Implicits extends Implicits

}
