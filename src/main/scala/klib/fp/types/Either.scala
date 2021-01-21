package klib.fp.types

import klib.fp.typeclass.{ForEach, Monad}

sealed trait Either[+A, +B] {

  def toOption: Option[B] =
    this match {
      case Right(b) => scala.Some(b)
      case _        => scala.None
    }

  def toSEither: scala.Either[A, B] =
    this match {
      case Right(b) =>
        scala.Right(b)
      case Left(a) =>
        scala.Left(a)
    }

  def toMaybe: Maybe[B] =
    this match {
      case Right(b) => Some(b)
      case _        => None
    }

}

final case class Right[+B](b: B) extends Either[Nothing, B]
final case class Left[+A](a: A) extends Either[A, Nothing]

object Either {

  trait Implicits {

    implicit class EitherIdOps[A](a: A) {

      def left: Either[A, Nothing] =
        Left(a)

      def right: Either[Nothing, A] =
        Right(a)

    }

  }
  object Implicits extends Implicits

  // Instances

  type Projection[A] = { type T[B] = Either[A, B] }

  implicit def eitherMonad[L]: Monad[Projection[L]#T] =
    new Monad[Projection[L]#T] {

      override def map[A, B](t: Either[L, A], f: A => B): Either[L, B] =
        t match {
          case Right(b) =>
            Right(f(b))
          case l @ Left(_) =>
            l
        }

      override def apply[A, B](t: Either[L, A], f: Either[L, A => B]): Either[L, B] =
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

      override def pure[A](a: => A): Either[L, A] =
        Right(a)

      override def flatten[A](t: Either[L, Either[L, A]]): Either[L, A] =
        t match {
          case Right(t)    => t
          case l @ Left(_) => l
        }

    }

  implicit def eitherForEach[L]: ForEach[Projection[L]#T] =
    new ForEach[Projection[L]#T] {

      override def forEach[A](t: L \/ A, f: A => Unit): Unit =
        t match {
          case Right(b) =>
            f(b)
          case Left(_) =>
        }

    }

}
