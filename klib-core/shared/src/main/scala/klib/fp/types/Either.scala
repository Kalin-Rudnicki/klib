package klib.fp.types

import klib.fp.typeclass._

sealed trait Either[+A, +B] {

  final def toOption: Option[B] =
    this match {
      case Right(b) => scala.Some(b)
      case _        => scala.None
    }

  final def toEA: ErrorAccumulator[A, B] =
    this match {
      case Right(b) =>
        Alive(b)
      case Left(a) =>
        Dead(a :: Nil)
    }

  final def toSEither: scala.Either[A, B] =
    this match {
      case Right(b) =>
        scala.Right(b)
      case Left(a) =>
        scala.Left(a)
    }

  final def toMaybe: Maybe[B] =
    this match {
      case Right(b) => Some(b)
      case _        => None
    }

}
type \/[+A, +B] = Either[A, B]

final case class Right[+B](b: B) extends Either[Nothing, B]
final case class Left[+A](a: A) extends Either[A, Nothing]

object Either {

  object extensions {

    extension [I](i: I) {

      def left: Either[I, Nothing] =
        Left(i)

      def right: Either[Nothing, I] =
        Right(i)

    }

  }

  object instances {

    given eitherMonad[L]: Monad[[R] =>> Either[L, R]] with {

      extension [R](t: Either[L, R]) {

        def map[B](f: R => B): Either[L, B] =
          t match {
            case Right(value) =>
              Right(f(value))
            case l @ Left(_) =>
              l
          }

        def apply[B](f: Either[L, R => B]): Either[L, B] =
          t match {
            case Right(t) =>
              f match {
                case Right(f) =>
                  Right(f(t))
                case l @ Left(_) =>
                  l
              }
            case l @ Left(_) =>
              l
          }

      }

      def pure[I](i: => I): Either[L, I] =
        Right(i)

      extension [A](t: Either[L, Either[L, A]])
        def flatten: Either[L, A] =
          t match {
            case Right(t)    => t
            case l @ Left(_) => l
          }

    }

    given eitherForeach[L]: Foreach[[R] =>> Either[L, R]] with {

      extension [R](t: Either[L, R])
        def foreach(f: R => Unit): Unit =
          t match {
            case Right(value) => f(value)
            case Left(_)      =>
          }

    }

  }

}
