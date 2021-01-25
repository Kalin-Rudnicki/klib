package klib.fp.types

import klib.fp.typeclass._

import scala.annotation.tailrec

sealed trait ErrorAccumulator[+E, +W, +R] {

  val warnings: List[W]

  def withWarning[W2 >: W](w: W2): ErrorAccumulator[E, W2, R] =
    this match {
      case Alive(r, warnings) =>
        Alive(r, w :: warnings)
      case Dead(errors, warnings) =>
        Dead(errors, w :: warnings)
    }

  def withWarnings[W2 >: W](ws: W2*): ErrorAccumulator[E, W2, R] =
    this match {
      case Alive(r, warnings) =>
        Alive(r, ws.toList ::: warnings)
      case Dead(errors, warnings) =>
        Dead(errors, ws.toList ::: warnings)
    }

  def toTuple: (Maybe[R], List[W], List[E]) =
    this match {
      case Alive(r, warnings) =>
        (Some(r), warnings, Nil)
      case Dead(errors, warnings) =>
        (None, warnings, errors)
    }

  def wrap[T[+_]: Applicative]: WrappedErrorAccumulator[T, E, W, R] =
    new WrappedErrorAccumulator(implicitly[Applicative[T]].pure(this))

}

final case class Alive[+W, +R](r: R, warnings: List[W] = Nil) extends ErrorAccumulator[Nothing, W, R]
final case class Dead[+E, +W](errors: List[E], warnings: List[W] = Nil) extends ErrorAccumulator[E, W, Nothing]

object ErrorAccumulator {

  trait Implicits {

    implicit class ErrorAccumulatorIdOps[A](a: A) {

      def alive: ErrorAccumulator[Nothing, Nothing, A] =
        Alive(a)

      def aliveIf[E](f: A => Boolean)(ifNot: E*): ErrorAccumulator[E, Nothing, A] =
        if (f(a))
          Alive(a)
        else
          Dead(ifNot.toList)

    }

  }
  object Implicits extends Implicits

  // Instances

  type Projection[E, W] = { type T[R] = ErrorAccumulator[E, W, R] }

  implicit def errorAccumulatorMonad[E, W]: Monad[Projection[E, W]#T] =
    new Monad[Projection[E, W]#T] {

      override def map[A, B](t: ErrorAccumulator[E, W, A], f: A => B): ErrorAccumulator[E, W, B] =
        t match {
          case Alive(t, warnings) => Alive(f(t), warnings)
          case d @ Dead(_, _)     => d
        }

      override def apply[A, B](t: ErrorAccumulator[E, W, A], f: ErrorAccumulator[E, W, A => B]): ErrorAccumulator[E, W, B] =
        (t, f) match {
          case (Alive(t, tWarnings), Alive(f, fWarnings)) =>
            Alive(f(t), tWarnings ::: fWarnings)
          case (Dead(tErrors, tWarnings), Dead(fErrors, fWarnings)) =>
            Dead(tErrors ::: fErrors, tWarnings ::: fWarnings)
          case (Alive(_, tWarnings), Dead(fErrors, fWarnings)) =>
            Dead(fErrors, tWarnings ::: fWarnings)
          case (Dead(tErrors, tWarnings), Alive(_, fWarnings)) =>
            Dead(tErrors, tWarnings ::: fWarnings)
        }

      override def pure[A](a: => A): ErrorAccumulator[E, W, A] =
        Alive(a, Nil)

      override def flatten[A](t: ErrorAccumulator[E, W, ErrorAccumulator[E, W, A]]): ErrorAccumulator[E, W, A] =
        t match {
          case Alive(t, warnings1) =>
            t match {
              case Alive(t, warnings2) =>
                Alive(t, warnings1 ::: warnings2)
              case Dead(errors, warnings2) =>
                Dead(errors, warnings1 ::: warnings2)
            }
          case d @ Dead(_, _) =>
            d
        }

    }

  implicit def errorAccumulatorForEach[E, W]: ForEach[Projection[E, W]#T] =
    new ForEach[Projection[E, W]#T] {

      override def forEach[A](t: ErrorAccumulator[E, W, A], f: A => Unit): Unit =
        t match {
          case Alive(r, _) =>
            f(r)
          case Dead(_, _) =>
        }

    }

  implicit def errorAccumulatorTraverseList[E, W]: Traverse[List, Projection[E, W]#T] =
    new Traverse[List, Projection[E, W]#T] {

      override def traverse[T](t: List[ErrorAccumulator[E, W, T]]): ErrorAccumulator[E, W, List[T]] = {
        @tailrec
        def loop(
            ea: ErrorAccumulator[E, W, List[T]],
            queue: List[ErrorAccumulator[E, W, T]],
        ): ErrorAccumulator[E, W, List[T]] =
          queue match {
            case Nil =>
              ea match {
                case Alive(r, warnings) =>
                  Alive(r.reverse, warnings)
                case d @ Dead(_, _) =>
                  d
              }
            case h :: tail =>
              loop(
                (ea, h) match {
                  case (Alive(eaR, eaWs), Alive(hR, hWs)) =>
                    Alive(hR :: eaR, eaWs ::: hWs)
                  case (Dead(eaEs, eaWs), Dead(hEs, hWs)) =>
                    Dead(eaEs ::: hEs, eaWs ::: hWs)
                  case (Alive(_, eaWs), Dead(hEs, hWs)) =>
                    Dead(hEs, eaWs ::: hWs)
                  case (Dead(eaEs, eaWs), Alive(_, hWs)) =>
                    Dead(eaEs, eaWs ::: hWs)
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

}
