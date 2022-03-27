package klib.utils

import cats.data.NonEmptyList
import cats.syntax.list.*
import cats.syntax.option.*
import zio.*
import zio.stream.*

// format: off
type ZStreamIteratorM[-R, +E, +A] = ZStreamIterator[R, KError[E], A]

type TaskStreamIteratorM[        +A] = ZStreamIteratorM[Any, Nothing, A]
type     StreamIteratorM[    +E, +A] = ZStreamIteratorM[Any, E,       A]
type    RStreamIteratorM[-R,     +A] = ZStreamIteratorM[R,   Nothing, A]

type     UStreamIterator[    +A] = ZStreamIterator[Any, Nothing, A]
type    URStreamIterator[-R, +A] = ZStreamIterator[R,   Nothing, A]
// format: on

final class ZStreamIterator[-R, +E, +A] private (state: Ref[ZStreamIterator.State[R, E, A]]) {

  private def open(reservation: Reservation[R, Nothing, Iterator[Either[E, A]]]): URIO[R, Unit] =
    reservation.acquire.flatMap(iter => state.set(ZStreamIterator.State.Opened(iter, reservation.release(Exit.unit))))

  private def close(release: URIO[R, Any]): URIO[R, Any] =
    release *> state.set(ZStreamIterator.State.Closed)

  private def fold[R1 <: R, E1 >: E, B](num: Int)(init: => B, onClosed: => ZIO[R1, E1, B])(
      onValue: (B, A) => ZIO[R1, E1, B],
  )(
      onComplete: B => ZIO[R1, E1, B],
  ): ZIO[R1, E1, B] =
    state.get.flatMap {
      case ZStreamIterator.State.Opened(iter, release) =>
        def rec(num: Int, b: B): ZIO[R1, E1, B] =
          if (num > 0)
            iter.nextOptionZIO.flatMap {
              case Some(next) =>
                next match {
                  case Right(value) => onValue(b, value).flatMap(rec(num - 1, _))
                  case Left(error)  => ZIO.fail(error)
                }
              case None => close(release) *> onComplete(b)
            }
          else
            ZIO.succeed(b)

        rec(num, init)
          .tapError { _ => close(release) }
      case ZStreamIterator.State.Closed =>
        onClosed
      case ZStreamIterator.State.NotOpened(reservation) =>
        open(reservation) *> fold(num)(init, onClosed)(onValue)(onComplete)
    }

  def stateType: UIO[ZStreamIterator.StateType] =
    state.get.map {
      case ZStreamIterator.State.NotOpened(_) => ZStreamIterator.StateType.NotOpened
      case ZStreamIterator.State.Opened(_, _) => ZStreamIterator.StateType.Opened
      case ZStreamIterator.State.Closed       => ZStreamIterator.StateType.Closed
    }

  def take(num: Int): ZIO[R, E, Option[NonEmptyList[A]]] =
    fold(num)(List.empty[A], ZIO.succeed(List.empty[A])) { (list, a) => ZIO.succeed(a :: list) }(ZIO.succeed)
      .map(_.reverse.toNel)

  /**
    * Executes the given effect on the first {num} number of elements, and leaves the remaining elements to be taken later
    */
  def takeForeach[R1 <: R, E1 >: E](num: Int)(f: A => ZIO[R1, E1, Any]): ZIO[R1, E1, Boolean] =
    fold(num)(true, ZIO.succeed(false)) { (_, a) => f(a).as(true) } { _ => ZIO.succeed(false) }

}
object ZStreamIterator {

  enum StateType(val canContinue: Boolean) {
    case NotOpened extends StateType(true)
    case Opened extends StateType(true)
    case Closed extends StateType(false)
  }

  private enum State[-R, +E, +A] {
    case NotOpened(reservation: Reservation[R, Nothing, Iterator[Either[E, A]]])
    case Opened(iter: Iterator[Either[E, A]], release: URIO[R, Any])
    case Closed
  }

  def fromZStream[R, E, A](zStream: ZStream[R, E, A]): UIO[ZStreamIterator[R, E, A]] =
    for {
      reservation <- zStream.toIterator.reserve
      state <- Ref.make(State.NotOpened(reservation))
    } yield ZStreamIterator(state)

}
