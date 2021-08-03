package klib.fp.types

import scala.annotation.tailrec
import scala.collection._

import klib.fp.typeclass._

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

final case class Some[+A](a: A) extends Maybe[A] {}
case object None extends Maybe[Nothing]

object Maybe {

  def apply[A](a: A): Maybe[A] =
    if (a == null)
      None
    else
      Some(a)

  def empty[A]: Maybe[A] =
    None

  trait Implicits {

    implicit class MaybeIdOps[A](a: A) {

      def some: Maybe[A] =
        Some(a)

      def ensure(f: A => Boolean): Maybe[A] =
        if (f(a))
          Some(a)
        else
          None

    }

  }
  object Implicits extends Implicits

  // Instances

  implicit val maybeMonad: Monad[Maybe] =
    new Monad[Maybe] {

      override def map[A, B](t: Maybe[A], f: A => B): Maybe[B] =
        t match {
          case Some(t) => Some(f(t))
          case None    => None
        }

      override def apply[A, B](t: Maybe[A], f: Maybe[A => B]): Maybe[B] =
        t match {
          case Some(t) =>
            f match {
              case Some(f) => Some(f(t))
              case None    => None
            }
          case None => None
        }

      override def pure[A](a: => A): Maybe[A] =
        Some(a)

      override def flatMap[A, B](t: Maybe[A], f: A => Maybe[B]): Maybe[B] =
        t match {
          case Some(a) => f(a)
          case None    => None
        }

    }

  implicit val maybeForEach: Foreach[Maybe] =
    new Foreach[Maybe] {

      override def foreach[A](t: Maybe[A], f: A => Unit): Unit =
        t match {
          case Some(a) =>
            f(a)
          case None =>
        }

    }

  implicit val maybeTraverseList: Traverse[List, Maybe] =
    new Traverse[List, Maybe] {

      override def traverse[T](t: List[Maybe[T]]): Maybe[List[T]] = {
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

  implicit def applicativeTraverseMaybe[A[_]](implicit aApplicative: Applicative[A]): Traverse[Maybe, A] =
    new Traverse[Maybe, A] {

      override def traverse[T](t: Maybe[A[T]]): A[Maybe[T]] =
        t match {
          case Some(a) => aApplicative.map(a, (a: T) => Some(a))
          case None    => aApplicative.pure(None)
        }

    }

}
