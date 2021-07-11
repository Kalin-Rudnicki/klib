package klib.fp.types

import klib.extensions.{given, _}
import klib.fp.types.ErrorAccumulator.instances.errorAccumulatorMonad
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

final case class Alive[+R](r: R) extends ErrorAccumulator[Nothing, R]
final case class Dead[+E](errors: List[E]) extends ErrorAccumulator[E, Nothing]

type ??[+R] = ErrorAccumulator[Throwable, R]
object ?? {

  object extensions {

    extension [A](t: ??[A])
      def toIO: IO[A] =
        IO.wrapEffect { t }

  }

  def apply[R](r: => R): ??[R] = r.pure[??]

  def dead(throwables: Throwable*): ??[Nothing] =
    Dead(throwables.toList)

}

object ErrorAccumulator {

  object extensions {

    extension [I](i: I) {

      def alive: ErrorAccumulator[Nothing, I] =
        Alive(i)

      def aliveIf[E](test: I => Boolean)(ifNot: E*): ErrorAccumulator[E, I] =
        if (test(i))
          Alive(i)
        else
          Dead(ifNot.toList)

    }

  }

  object instances {

    given errorAccumulatorMonad[E]: Monad[[A] =>> ErrorAccumulator[E, A]] with {

      extension [A](t: ErrorAccumulator[E, A]) {

        def map[B](f: A => B): ErrorAccumulator[E, B] =
          t match {
            case Alive(t)    => Alive(f(t))
            case d @ Dead(_) => d
          }

        def apply[B](f: ErrorAccumulator[E, A => B]): ErrorAccumulator[E, B] =
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

      }

      def pure[I](i: => I): ErrorAccumulator[E, I] =
        Alive(i)

      extension [A](t: ErrorAccumulator[E, ErrorAccumulator[E, A]])
        def flatten: ErrorAccumulator[E, A] =
          t match {
            case Alive(t2)   => t2
            case d @ Dead(_) => d
          }

    }

    given errorAccumulatorForeach[E]: Foreach[[A] =>> ErrorAccumulator[E, A]] with {

      extension [A](t: ErrorAccumulator[E, A])
        def foreach(f: A => Unit): Unit =
          t match {
            case Alive(value) => f(value)
            case Dead(_)      =>
          }

    }

    given errorAccumulatorTraverseList[E]: Traverse[List, [A] =>> ErrorAccumulator[E, A]] with {

      extension [T](t: List[ErrorAccumulator[E, T]])
        def traverse: ErrorAccumulator[E, List[T]] = {
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

    given errorAccumulatorTraverseNonEmptyList[E]: Traverse[NonEmptyList, [A] =>> ErrorAccumulator[E, A]] with {

      extension [A](t: NonEmptyList[ErrorAccumulator[E, A]])
        def traverse: ErrorAccumulator[E, NonEmptyList[A]] = {
          @tailrec
          def loop(
              ea: ErrorAccumulator[E, NonEmptyList[A]],
              queue: List[ErrorAccumulator[E, A]],
          ): ErrorAccumulator[E, NonEmptyList[A]] =
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

}
