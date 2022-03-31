package klib.web

import monocle.Lens
import org.scalajs.dom.window
import zio.*

import klib.utils.*

sealed abstract class RaiseHandler[-A, -S](runtime: Runtime[Executable.BaseEnv]) { self =>

  // =====| To Implement  |=====

  protected def handleRaise(raise: Raise[A, S]): STaskM[Unit]

  // =====| Public API  |=====

  inline final def raise(raise: Raise[A, S]*): Unit = raiseManyZIO(ZIO.succeed(raise.toList))
  inline final def raiseMany(raise: List[Raise[A, S]]): Unit = raiseManyZIO(ZIO.succeed(raise))
  inline final def raiseZIO(raise: STaskM[Raise[A, S]]): Unit = raiseManyZIO(raise.map(_ :: Nil))
  final def raiseManyZIO(raise: STaskM[List[Raise[A, S]]]): Unit =
    runtime.unsafeRun {
      raise
        .flatMap {
          ZIO.foreachDiscard(_)(handleRaise)
        }
        .dumpErrorsAndContinue(Logger.LogLevel.Error)
    }

  inline final def modifyState[S2 <: S](modify: S2 => S2): Unit = raise(Raise.modifyState(modify))
  inline final def modifyStateNoReRender[S2 <: S](modify: S2 => S2): Unit = raise(Raise.modifyStateNoReRender(modify))
  inline final def setState(set: => S): Unit = raise(Raise.setState(set))
  inline final def setStateNoReRender(set: => S): Unit = raise(Raise.setStateNoReRender(set))

  inline final def raiseAction(raise: A*): Unit = raiseMany(raise.map(Raise.Action(_)).toList)
  inline final def raiseActionMany(raise: List[A]): Unit = raiseMany(raise.map(Raise.Action(_)))
  inline final def raiseActionZIO(raise: STaskM[A]): Unit = raiseZIO(raise.map(Raise.Action(_)))
  inline final def raiseActionManyZIO(raise: STaskM[List[A]]): Unit = raiseManyZIO(raise.map(_.map(Raise.Action(_))))

  // =====| Helpers |=====

  private[web] final def mapRaise[NewA, NewS](f: Raise[NewA, NewS] => STaskM[List[Raise[A, S]]]): RaiseHandler[NewA, NewS] =
    RaiseHandler[NewA, NewS](
      f(_).flatMap(ZIO.foreach(_)(handleRaise)).unit,
      runtime,
    )

  private[web] final def mapState[S2 <: S, NewS](lens: Lens[S2, NewS]): RaiseHandler[A, NewS] =
    mapRaise[A, NewS] {
      case modifyState: Raise.ModifyState[NewS] => ZIO.succeed(modifyState.mapState(lens) :: Nil)
      case standard: Raise.Standard             => ZIO.succeed(standard :: Nil)
      case action: Raise.Action[A]              => ZIO.succeed(action :: Nil)
    }

  private[web] final def mapAction[NewA](f: NewA => STaskM[List[Raise[A, S]]]): RaiseHandler[NewA, S] =
    mapRaise[NewA, S] {
      case modifyState: Raise.ModifyState[S] => ZIO.succeed(modifyState :: Nil)
      case standard: Raise.Standard          => ZIO.succeed(standard :: Nil)
      case action: Raise.Action[NewA]        => f(action.action)
    }

}
object RaiseHandler {

  def apply[A, S](
      _handleRaise: Raise[A, S] => STaskM[Unit],
      _runtime: Runtime[Executable.BaseEnv],
  ): RaiseHandler[A, S] =
    new RaiseHandler[A, S](_runtime) {
      override protected def handleRaise(raise: Raise[A, S]): STaskM[Unit] = _handleRaise(raise)
    }

  private[web] def root[A, S](
      renderer: VDomActions.Renderer,
      envRef: Ref.Synchronized[S],
      widget: AVWidget[A, S, Any],
      handleA: A => STaskM[List[Raise.StandardOrUpdate[S]]],
      titleF: Page.TitleF[S],
      runtime: Runtime[Executable.BaseEnv],
  ): RaiseHandler[A, S] =
    new RaiseHandler[A, S](runtime) {

      override protected def handleRaise(raise: Raise[A, S]): STaskM[Unit] = {
        def handleStandardOrUpdate(standardOrUpdate: Raise.StandardOrUpdate[S]): STaskM[Unit] =
          standardOrUpdate match {
            case standard: Raise.Standard =>
              standard match {
                case Raise.Standard.DisplayMessage(message, _) => Logger.println.info(s"DisplayMessage: $message")
                case history: Raise.Standard.History =>
                  history match {
                    case Raise.Standard.History.Push(page)    => page().push(renderer, runtime)
                    case Raise.Standard.History.Replace(page) => page().replace(renderer, runtime)
                    case Raise.Standard.History.Go(delta)     => ZIO.fail(KError.message.???) // TODO: I believe this needs to have access to the RouteMatcher
                  }
                case Raise.Standard.ReRender =>
                  for {
                    env <- envRef.get
                    newVDom = widget.elements(this, env)
                    _ <- renderer.render(titleF(env), newVDom)
                  } yield ()
              }
            case modifyState: Raise.ModifyState[S] =>
              envRef.updateZIO { env =>
                val newEnv = modifyState.modify(env)
                val newVDom = widget.elements(this, newEnv)
                if (modifyState.reRender) renderer.render(titleF(newEnv), newVDom).as(newEnv)
                else ZIO.succeed(newEnv)
              }
          }

        raise match {
          case sou: Raise.StandardOrUpdate[S] => handleStandardOrUpdate(sou)
          case Raise.Action(action)           => handleA(action).flatMap(ZIO.foreachDiscard(_)(handleStandardOrUpdate))
        }
      }

    }

}

sealed trait Raise[+A, +S]
object Raise {

  sealed trait StandardOrUpdate[+S] extends Raise[Nothing, S]

  final case class ModifyState[S](modify: S => S, reRender: Boolean) extends StandardOrUpdate[S] {
    def mapState[S2](lens: Lens[S2, S]): ModifyState[S2] =
      ModifyState(lens.modify(modify), reRender)
  }

  sealed trait Standard extends StandardOrUpdate[Nothing]
  object Standard {

    final case class DisplayMessage(message: String, modifier: VDom.Modifier) extends Standard

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

  }

  final case class Action[A](action: A) extends Raise[A, Nothing]

  inline def modifyState[S](modify: S => S): ModifyState[S] = ModifyState(modify, true)
  inline def setState[S](set: => S): ModifyState[S] = ModifyState(_ => set, true)

  inline def modifyStateNoReRender[S](modify: S => S): ModifyState[S] = ModifyState(modify, false)
  inline def setStateNoReRender[S](set: => S): ModifyState[S] = ModifyState(_ => set, false)

}
