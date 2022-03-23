package klib.web

import cats.data.NonEmptyList
import monocle.Lens
import zio.*

import klib.utils.*

trait RaiseHandler[-A, -S] {

  inline final def raise(raise: Raise[A, S]): Unit = raiseManyNelZIO(ZIO.succeed(raise :: Nil))
  inline final def raiseMany(raise: List[Raise[A, S]]): Unit = raiseManyNelZIO(ZIO.succeed(raise))
  inline final def raiseZIO(raise: IO[Message, Raise[A, S]]): Unit = raiseManyNelZIO(raise.mapBoth(NonEmptyList.one(_), _ :: Nil))
  inline final def raiseNelZIO(raise: IO[NonEmptyList[Message], Raise[A, S]]): Unit = raiseManyNelZIO(raise.map(_ :: Nil))
  inline final def raiseManyZIO(raise: IO[Message, List[Raise[A, S]]]): Unit = raiseManyNelZIO(raise.mapError(NonEmptyList.one(_)))
  final def raiseManyNelZIO(raise: IO[NonEmptyList[Message], List[Raise[A, S]]]): Unit = ??? // TODO:

  final def mapState[S2 <: S, NewS](f: Lens[S2, NewS]): RaiseHandler[A, NewS] = ???

}

sealed trait Raise[+A, +S]
object Raise {

  final case class ModifyState[S](modify: S => S) extends Raise[Nothing, S]

  final case class Action[A](action: A) extends Raise[A, Nothing]

  sealed trait Standard extends Raise[Nothing, Nothing]
  object Standard {

    final case class DisplayMessage(message: String, modifier: VDom.Modifier) extends Standard

    type Page = Unit // TODO:
    sealed trait History extends Standard
    object History {
      final case class Push(page: () => Page) extends History
      final case class Replace(page: () => Page) extends History
      final case class Go(delta: Int) extends History

      inline def push(page: => Page): Push = Push(() => page)
      inline def replace(page: => Page): Replace = Replace(() => page)

      inline def go(delta: Int): Go = Go(delta)
      inline def forward: Go = go(1)
      inline def back: Go = go(-1)
    }

    case object ReRender extends Standard

    // TODO: Raw

  }

  inline def modifyState[S](modify: S => S): ModifyState[S] = ModifyState(modify)
  inline def setState[S](set: => S): ModifyState[S] = ModifyState(_ => set)

}
