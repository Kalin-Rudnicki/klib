package klib.fp.types

import klib.fp.typeclass._

import scala.annotation.tailrec

sealed trait ErrorAccumulator[+E, +R] {

  def mapErrors[E2](mapE: E => E2): ErrorAccumulator[E2, R] =
    this match {
      case alive @ Alive(_) =>
        alive
      case Dead(errors) =>
        Dead(errors.map(mapE))
    }

}

final case class Alive[+W, +R](r: R) extends ErrorAccumulator[Nothing, R]
final case class Dead[+E](errors: List[E]) extends ErrorAccumulator[E, Nothing]

object ErrorAccumulator {

  trait Implicits {

    implicit class ErrorAccumulatorIdOps[A](a: A) {

      def alive: ErrorAccumulator[Nothing, A] =
        Alive(a)

      def aliveIf[E](f: A => Boolean)(ifNot: E*): ErrorAccumulator[E, A] =
        if (f(a))
          Alive(a)
        else
          Dead(ifNot.toList)

    }

    implicit class `?Ops`[T](t: ?[T]) {

      def to_?? : ??[T] =
        new ??(IO(t))

    }

  }
  object Implicits extends Implicits

  // Instances

  type Projection[E] = { type T[R] = ErrorAccumulator[E, R] }

  implicit def errorAccumulatorMonad[E]: Monad[Projection[E]#T] =
    new Monad[Projection[E]#T] {

      override def map[A, B](t: ErrorAccumulator[E, A], f: A => B): ErrorAccumulator[E, B] =
        t match {
          case Alive(t)    => Alive(f(t))
          case d @ Dead(_) => d
        }

      override def apply[A, B](t: ErrorAccumulator[E, A], f: ErrorAccumulator[E, A => B]): ErrorAccumulator[E, B] =
        (t, f) match {
          case (Alive(t), Alive(f)) =>
            Alive(f(t))
          case (Dead(tErrors), Dead(fErrors)) =>
            Dead(tErrors ::: fErrors)
          case (Alive(_), Dead(fErrors)) =>
            Dead(fErrors)
          case (Dead(tErrors), Alive(_)) =>
            Dead(tErrors)
        }

      override def pure[A](a: => A): ErrorAccumulator[E, A] =
        Alive(a)

      override def flatten[A](t: ErrorAccumulator[E, ErrorAccumulator[E, A]]): ErrorAccumulator[E, A] =
        t match {
          case Alive(t) =>
            t match {
              case Alive(t) =>
                Alive(t)
              case d @ Dead(errors) =>
                d
            }
          case d @ Dead(_) =>
            d
        }

    }

  implicit def errorAccumulatorForEach[E]: Foreach[Projection[E]#T] =
    new Foreach[Projection[E]#T] {

      override def foreach[A](t: ErrorAccumulator[E, A], f: A => Unit): Unit =
        t match {
          case Alive(r) =>
            f(r)
          case Dead(_) =>
        }

    }

  implicit def errorAccumulatorTraverseList[E]: Traverse[List, Projection[E]#T] =
    new Traverse[List, Projection[E]#T] {

      override def traverse[T](t: List[ErrorAccumulator[E, T]]): ErrorAccumulator[E, List[T]] = {
        @tailrec
        def loop(
            ea: ErrorAccumulator[E, List[T]],
            queue: List[ErrorAccumulator[E, T]],
        ): ErrorAccumulator[E, List[T]] =
          queue match {
            case Nil =>
              ea match {
                case Alive(r) =>
                  Alive(r.reverse)
                case d @ Dead(_) =>
                  d
              }
            case h :: tail =>
              loop(
                (ea, h) match {
                  case (Alive(eaR), Alive(hR)) =>
                    Alive(hR :: eaR)
                  case (Dead(eaEs), Dead(hEs)) =>
                    Dead(eaEs ::: hEs)
                  case (Alive(_), Dead(hEs)) =>
                    Dead(hEs)
                  case (Dead(eaEs), Alive(_)) =>
                    Dead(eaEs)
                },
                tail,
              )
          }

        loop(
          Alive(Nil),
          t,
        )
      }

    }

  implicit def errorAccumulatorTraverseNonEmptyList[E]: Traverse[NonEmptyList, Projection[E]#T] =
    new Traverse[NonEmptyList, Projection[E]#T] {

      override def traverse[T](t: NonEmptyList[ErrorAccumulator[E, T]]): ErrorAccumulator[E, NonEmptyList[T]] = {
        @tailrec
        def loop(
            ea: ErrorAccumulator[E, NonEmptyList[T]],
            queue: List[ErrorAccumulator[E, T]],
        ): ErrorAccumulator[E, NonEmptyList[T]] =
          // TODO (KR) : duplicated, add trait?
          queue match {
            case Nil =>
              ea match {
                case Alive(r) =>
                  Alive(r.reverse)
                case d @ Dead(_) =>
                  d
              }
            case h :: tail =>
              loop(
                (ea, h) match {
                  case (Alive(eaR), Alive(hR)) =>
                    Alive(hR :: eaR)
                  case (Dead(eaEs), Dead(hEs)) =>
                    Dead(eaEs ::: hEs)
                  case (Alive(_), Dead(hEs)) =>
                    Dead(hEs)
                  case (Dead(eaEs), Alive(_)) =>
                    Dead(eaEs)
                },
                tail,
              )
          }

        t.head match {
          case Alive(r) =>
            loop(
              Alive(NonEmptyList(r, Nil)),
              t.tail,
            )
          case d @ Dead(_) =>
            loop(
              d,
              t.tail,
            )
        }
      }

    }

}
