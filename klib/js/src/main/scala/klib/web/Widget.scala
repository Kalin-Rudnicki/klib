package klib.web

import cats.data.*
import cats.syntax.either.*
import monocle.*
import scala.annotation.targetName
import scala.quoted.*
import zio.*

import klib.utils.*

trait PWidget[+Action, -StateGet, +StateSet, +Value] { self =>

  // =====| Helpers |=====

  val elements: (RaiseHandler[Action, StateSet], StateGet) => List[VDom.Element]
  val value: StateGet => Valid[Value]

  // =====| Combinators |=====

  final def placeBefore[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    PWidget.many(
      (rh, s) => self.elements(rh, s) ::: other.elements(rh, s),
      s => self.value(s) accumulate other.value(s),
    )
  inline final def >>[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    placeBefore(other)

  final def placeAfter[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value2, Value],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    PWidget.many(
      (rh, s) => other.elements(rh, s) ::: self.elements(rh, s),
      s => other.value(s) accumulate self.value(s),
    )
  inline final def <<[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value2, Value],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    placeAfter(other)

  @targetName("wrappedMany")
  final def wrapped(wrapInner: List[VDom.Element] => List[VDom.Element]): PWidget[Action, StateGet, StateSet, Value] =
    PWidget.many(
      (rh, s) => wrapInner(elements(rh, s)),
      value,
    )

  inline final def wrapped(wrapInner: List[VDom.Element] => VDom.Element): PWidget[Action, StateGet, StateSet, Value] =
    wrapped(wrapInner(_) :: Nil)

  // =====| Mapping |=====

  final def mapEitherValue[Value2](f: Valid[Value] => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    PWidget.many(
      elements,
      s => f(value(s)),
    )

  inline final def mapValue[Value2](f: Value => Value2): PWidget[Action, StateGet, StateSet, Value2] =
    mapEitherValue(_.map(f))

  inline final def flatMapValue[Value2](f: Value => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    mapEitherValue(_.flatMap(f))

  final def imapState[OuterState, InnerState >: StateSet <: StateGet](
      lens: Lens[OuterState, InnerState],
  ): AVWidget[Action, OuterState, Value] =
    PWidget.many(
      (rh, s) => elements(rh.mapState(lens), lens.get(s)),
      s => value(lens.get(s)),
    )

}
object PWidget {

  def many[Action, StateGet, StateSet, Value](
      elementsF: (RaiseHandler[Action, StateSet], StateGet) => List[VDom.Element],
      valueF: StateGet => Valid[Value],
  ): PWidget[Action, StateGet, StateSet, Value] =
    new PWidget[Action, StateGet, StateSet, Value] {
      override val elements: (RaiseHandler[Action, StateSet], StateGet) => List[VDom.Element] = elementsF
      override val value: StateGet => Valid[Value] = valueF
    }

  inline def apply[Action, StateGet, StateSet, Value](
      elementsF: (RaiseHandler[Action, StateSet], StateGet) => VDom.Element,
      valueF: StateGet => Valid[Value],
  ): PWidget[Action, StateGet, StateSet, Value] =
    PWidget.many[Action, StateGet, StateSet, Value](elementsF(_, _) :: Nil, valueF)

}

// =====| Type Aliases |=====

type Valid[Value] = EitherNel[String, Value]

// =====| Stateless Widgets |=====

type CWidget = PWidget[Nothing, Any, Nothing, Unit]
object CWidget {

  def many(
      elementsF: => List[VDom.Element],
  ): CWidget =
    PWidget.many[Nothing, Any, Nothing, Unit](
      (_, _) => elementsF,
      _ => ().asRight,
    )

  inline def apply(
      elementsF: => VDom.Element,
  ): CWidget =
    CWidget.many(elementsF :: Nil)

}

type CAWidget[+Action] = PWidget[Action, Any, Nothing, Unit]
object CAWidget {

  def many[Action](
      elementsF: RaiseHandler[Action, Nothing] => List[VDom.Element],
  ): CAWidget[Action] =
    PWidget.many[Action, Any, Nothing, Unit](
      (rh, _) => elementsF(rh),
      _ => ().asRight,
    )

  inline def apply[Action](
      elementsF: RaiseHandler[Action, Nothing] => VDom.Element,
  ): CAWidget[Action] =
    CAWidget.many[Action](elementsF(_) :: Nil)

}

type CVWidget[+Value] = PWidget[Nothing, Any, Nothing, Value]
object CVWidget {

  def many[Value](
      elementsF: => List[VDom.Element],
      valueF: => Valid[Value],
  ): CVWidget[Value] =
    PWidget.many[Nothing, Any, Nothing, Value](
      (_, _) => elementsF,
      _ => valueF,
    )

  inline def apply[Value](
      elementsF: => VDom.Element,
      valueF: => Valid[Value],
  ): CVWidget[Value] =
    CVWidget.many[Value](elementsF :: Nil, valueF)

}

type CAVWidget[+Action, +Value] = PWidget[Action, Any, Nothing, Value]
object CAVWidget {

  def many[Action, Value](
      elementsF: RaiseHandler[Action, Nothing] => List[VDom.Element],
      valueF: => Valid[Value],
  ): CAVWidget[Action, Value] =
    PWidget.many[Action, Any, Nothing, Value](
      (rh, _) => elementsF(rh),
      _ => valueF,
    )

  inline def apply[Action, Value](
      elementsF: RaiseHandler[Action, Nothing] => VDom.Element,
      valueF: => Valid[Value],
  ): CAVWidget[Action, Value] =
    CAVWidget.many[Action, Value](elementsF(_) :: Nil, valueF)

}

// =====| Stateful Widgets |=====

type Widget[State] = PWidget[Nothing, State, State, Unit]
object Widget {

  def many[State](
      elementsF: State => List[VDom.Element],
  ): Widget[State] =
    PWidget.many[Nothing, State, State, Unit](
      (_, s) => elementsF(s),
      _ => ().asRight,
    )

  inline def apply[State](
      elementsF: State => VDom.Element,
  ): Widget[State] =
    Widget.many[State](elementsF(_) :: Nil)

}

type AWidget[+Action, State] = PWidget[Action, State, State, Unit]
object AWidget {

  def many[Action, State](
      elementsF: (RaiseHandler[Action, State], State) => List[VDom.Element],
  ): AWidget[Action, State] =
    PWidget.many[Action, State, State, Unit](
      elementsF,
      _ => ().asRight,
    )

  def apply[Action, State](
      elementsF: (RaiseHandler[Action, State], State) => VDom.Element,
  ): AWidget[Action, State] =
    AWidget.many[Action, State](elementsF(_, _) :: Nil)

}

type VWidget[State, +Value] = PWidget[Nothing, State, State, Value]
object VWidget {

  def many[State, Value](
      elementsF: State => List[VDom.Element],
      valueF: State => Valid[Value],
  ): VWidget[State, Value] =
    PWidget.many[Nothing, State, State, Value](
      (_, s) => elementsF(s),
      valueF,
    )

  inline def apply[State, Value](
      elementsF: State => VDom.Element,
      valueF: State => Valid[Value],
  ): VWidget[State, Value] =
    VWidget.many[State, Value](elementsF(_) :: Nil, valueF)

}

type AVWidget[+Action, State, +Value] = PWidget[Action, State, State, Value]
object AVWidget {

  def many[Action, State, Value](
      elementsF: (RaiseHandler[Action, State], State) => List[VDom.Element],
      valueF: State => Valid[Value],
  ): AVWidget[Action, State, Value] =
    PWidget.many[Action, State, State, Value](
      elementsF,
      valueF,
    )

  def apply[Action, State, Value](
      elementsF: (RaiseHandler[Action, State], State) => VDom.Element,
      valueF: State => Valid[Value],
  ): AVWidget[Action, State, Value] =
    AVWidget.many[Action, State, Value](elementsF(_, _) :: Nil, valueF)

}
