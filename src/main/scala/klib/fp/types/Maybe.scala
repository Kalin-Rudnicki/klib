package klib.fp.types

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

  def toEA[E](errs: E*): ErrorAccumulator[E, Nothing, A] =
    this match {
      case Some(a) =>
        Alive(a)
      case None =>
        Dead(errs.toList)
    }

  def <<?[E](errs: E*): ErrorAccumulator[E, Nothing, A] =
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

      override def flatten[A](t: Maybe[Maybe[A]]): Maybe[A] =
        t match {
          case Some(t) => t
          case None    => None
        }

    }

  implicit val maybeForEach: ForEach[Maybe] =
    new ForEach[Maybe] {

      override def forEach[A](t: Maybe[A], f: A => Unit): Unit =
        t match {
          case Some(a) =>
            f(a)
          case None =>
        }

    }

}
