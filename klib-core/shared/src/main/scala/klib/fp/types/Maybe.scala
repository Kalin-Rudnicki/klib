package klib.fp.types

import klib.fp.extensions.{given, _}
import klib.instances.{given, _}
import klib.fp.typeclass._

import scala.annotation.tailrec

sealed trait Maybe[+A] {

  def getOrElse[A2 >: A](a: A2): A2 =
    this match {
      case Some(a) => a
      case None    => a
    }

  def orElse[A2 >: A](a: Maybe[A2]): Maybe[A2] =
    this match {
      case Some(_) =>
        this
      case None =>
        a
    }

  def filter(f: A => Boolean): Maybe[A] =
    this match {
      case Some(a) if !f(a) =>
        None
      case _ =>
        this
    }
  def filterNot(f: A => Boolean): Maybe[A] =
    this match {
      case Some(a) if f(a) =>
        None
      case _ =>
        this
    }

  def toOption: Option[A] =
    this match {
      case Some(a) => scala.Some(a)
      case None    => scala.None
    }

  def toList: List[A] =
    this match {
      case Some(a) => a :: Nil
      case None    => Nil
    }

  def toSet[A2 >: A]: Set[A2] =
    this match {
      case Some(a) => Set(a)
      case None    => Set.empty
    }

  def cata[B](mapF: A => B, orElse: => B): B =
    this match {
      case Some(a) =>
        mapF(a)
      case None =>
        orElse
    }

  def toEA[E](errs: E*): ErrorAccumulator[E, A] =
    this match {
      case Some(a) =>
        Alive(a)
      case None =>
        Dead(errs.toList)
    }

  def <<?[E](errs: E*): ErrorAccumulator[E, A] =
    toEA(errs: _*)

  def isEmpty: Boolean =
    this match {
      case Some(_) =>
        false
      case None =>
        true
    }

  def nonEmpty: Boolean =
    this match {
      case Some(_) =>
        true
      case None =>
        false
    }

}

final case class Some[+A](a: A) extends Maybe[A]
case object None extends Maybe[Nothing]

object Maybe {

  def apply[A](a: A): Maybe[A] =
    if (a == null)
      None
    else
      Some(a)

  object extensions {

    extension [I](i: I) {

      def some: Maybe[I] =
        Some(i)

      def ensure(f: I => Boolean): Maybe[I] =
        if (f(i))
          Some(i)
        else
          None

    }

  }

  object instances {

    given maybeMonad: Monad[Maybe] with {

      extension [A](t: Maybe[A]) {

        def map[B](f: A => B): Maybe[B] =
          t match {
            case Some(value) => Some(f(value))
            case None        => None
          }

        def apply[B](f: Maybe[A => B]): Maybe[B] =
          t match {
            case Some(t) =>
              f match {
                case Some(f) => Some(f(t))
                case None    => None
              }
            case None => None
          }

      }

      def pure[I](i: => I): Maybe[I] =
        Some(i)

      extension [A](t: Maybe[Maybe[A]])
        def flatten: Maybe[A] =
          t match {
            case Some(t2) => t2
            case None     => None
          }

    }

    given maybeForeach: Foreach[Maybe] with {

      extension [A](t: Maybe[A])
        def foreach(f: A => Unit): Unit =
          t match {
            case Some(value) => f(value)
            case None        =>
          }

    }

    given maybeTraverseList: Traverse[List, Maybe] with {

      extension [T](t: List[Maybe[T]])
        def traverse: Maybe[List[T]] = {
          @tailrec
          def loop(queue: List[Maybe[T]], stack: List[T]): Maybe[List[T]] =
            queue match {
              case head :: tail =>
                head match {
                  case Some(h) =>
                    loop(tail, h :: stack)
                  case None =>
                    None
                }
              case Nil =>
                Some(stack.reverse)
            }

          loop(t, Nil)
        }

    }

    given applicativeTraverseMaybe[A[_]: Applicative]: Traverse[Maybe, A] with {

      extension [T](t: Maybe[A[T]])
        def traverse: A[Maybe[T]] =
          t match {
            case Some(a) => a.map(Some(_))
            case None    => None.pure
          }

    }

  }

}
