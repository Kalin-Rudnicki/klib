package klib.web

import cats.data.*
import cats.syntax.either.*
import monocle.*
import monocle.Focus.KeywordContext
import monocle.macros.GenLens
import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance
import scala.quoted.*
import zio.*

import klib.utils.*
import klib.web.VDomBuilders.CSSAttrBuilders.display

trait PWidget[+Action, -StateGet, +StateSet <: StateGet, +Value] { self =>

  // =====| Helpers |=====

  val elements: (RaiseHandler[Action, StateSet], StateGet) => List[VDom.Element]
  val value: StateGet => Valid[Value]

  // =====| Combinators |=====

  final def placeBefore[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    PWidget.many(
      (rh, s) => self.elements(rh, s) ::: other.elements(rh, s),
      s => self.value(s) accumulate other.value(s),
    )
  inline final def >>[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    placeBefore[Action2, StateGet2, StateSet2, Value2](other)

  final def placeAfter[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value2, Value],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    PWidget.many(
      (rh, s) => other.elements(rh, s) ::: self.elements(rh, s),
      s => other.value(s) accumulate self.value(s),
    )
  inline final def <<[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value2, Value],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    placeAfter[Action2, StateGet2, StateSet2, Value2](other)

  @targetName("wrappedMany")
  final def wrapped(wrapInner: List[VDom.Element] => List[VDom.Element]): PWidget[Action, StateGet, StateSet, Value] =
    PWidget.many(
      (rh, s) => wrapInner(elements(rh, s)),
      value,
    )

  inline final def wrapped(wrapInner: List[VDom.Element] => VDom.Element): PWidget[Action, StateGet, StateSet, Value] =
    wrapped(wrapInner(_) :: Nil)

  // =====| Mapping |=====

  // --- Map with Value ---

  final def mapEitherValue[Value2](f: Valid[Value] => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    PWidget.many(
      elements,
      s => f(value(s)),
    )

  inline final def mapValue[Value2](f: Value => Value2): PWidget[Action, StateGet, StateSet, Value2] =
    mapEitherValue(_.map(f))

  inline final def flatMapValue[Value2](f: Value => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    mapEitherValue(_.flatMap(f))

  // --- Map with Value & State ---

  final def flatMapEitherValueWithState[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      f: (StateGet2, Valid[Value]) => Valid[Value2],
  ): PWidget[Action, StateGet2, StateSet2, Value2] =
    PWidget.many[Action, StateGet2, StateSet2, Value2](
      elements,
      s => f(s, value(s)),
    )

  inline final def flatMapValueWithState[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      f: (StateGet2, Value) => Valid[Value2],
  ): PWidget[Action, StateGet2, StateSet2, Value2] =
    flatMapEitherValueWithState[StateGet2, StateSet2, Value2] { (s, v) => v.flatMap(f(s, _)) }

  inline final def mapValueWithState[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      f: (StateGet2, Value) => Value2,
  ): PWidget[Action, StateGet2, StateSet2, Value2] =
    flatMapEitherValueWithState[StateGet2, StateSet2, Value2] { (s, v) => v.map(f(s, _)) }

  // --- Map with State ---

  final def eitherValueFromState[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      f: StateGet2 => Valid[Value2],
  ): PWidget[Action, StateGet2, StateSet2, Value2] =
    PWidget.many[Action, StateGet2, StateSet2, Value2](
      elements,
      f(_),
    )

  inline final def valueFromState[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      f: StateGet2 => Value2,
  ): PWidget[Action, StateGet2, StateSet2, Value2] =
    eitherValueFromState[StateGet2, StateSet2, Value2](f(_).asRight)

  // --- Map with Nothing ---

  final def asEither[Value2](f: => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    mapEitherValue(_ => f)

  inline final def asError(f: => NonEmptyList[String]): PWidget[Action, StateGet, StateSet, Nothing] =
    asEither(f.asLeft)

  inline final def as[Value2](f: => Value2): PWidget[Action, StateGet, StateSet, Value2] =
    asEither(f.asRight)

  // --- Map State ---

  final def imapState[OuterState, InnerState >: StateSet <: StateGet](
      lens: Lens[OuterState, InnerState],
  ): AVWidget[Action, OuterState, Value] =
    PWidget.many(
      (rh, s) => elements(rh.mapState(lens), lens.get(s)),
      s => value(lens.get(s)),
    )

  // TODO (KR) : I cant get the stupid variance to work properly on this one
  //           : The problem is that this method can not take type-parameters, because the apply method
  //           : on PWidget.LensBuilder needs to take exactly 1 parameter, in order to be able to infer give the type hint
  final def zoomOut: PWidget.LensBuilder[Action, StateGet @uncheckedVariance, StateSet @uncheckedVariance, Value] =
    PWidget.LensBuilder(self)

  final def placeBeforeWithEitherValue[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: Valid[Value] => PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value2, Value],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    PWidget.many(
      (rh, s) => other(value(s)).elements(rh, s) ::: self.elements(rh, s),
      { s =>
        val svs = self.value(s)
        other(svs).value(s) accumulate svs
      },
    )

  inline final def placeBeforeWithValue[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: Value => PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value2, Value],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    placeBeforeWithEitherValue[Action2, StateGet2, StateSet2, Value2] {
      case Right(v) => other(v)
      case Left(_) =>
        PWidget[Action2, StateGet2, StateSet2, Value2](
          (_, _) => VDomBuilders.span(display := "none"),
          _ => NonEmptyList.one("placeBeforeWithValue: value is invalid").asLeft,
        )
    }

  final def placeAfterWithEitherValue[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: Valid[Value] => PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    PWidget.many(
      (rh, s) => self.elements(rh, s) ::: other(value(s)).elements(rh, s),
      { s =>
        val svs = self.value(s)
        svs accumulate other(svs).value(s)
      },
    )

  inline final def placeAfterWithValue[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: Value => PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    placeAfterWithEitherValue[Action2, StateGet2, StateSet2, Value2] {
      case Right(v) => other(v)
      case Left(_) =>
        PWidget[Action2, StateGet2, StateSet2, Value2](
          (_, _) => VDomBuilders.span(display := "none"),
          _ => NonEmptyList.one("placeAfterWithValue: value is invalid").asLeft,
        )
    }

}
object PWidget {

  def many[Action, StateGet, StateSet <: StateGet, Value](
      elementsF: (RaiseHandler[Action, StateSet], StateGet) => List[VDom.Element],
      valueF: StateGet => Valid[Value],
  ): PWidget[Action, StateGet, StateSet, Value] =
    new PWidget[Action, StateGet, StateSet, Value] {
      override val elements: (RaiseHandler[Action, StateSet], StateGet) => List[VDom.Element] = elementsF
      override val value: StateGet => Valid[Value] = valueF
    }

  inline def apply[Action, StateGet, StateSet <: StateGet, Value](
      elementsF: (RaiseHandler[Action, StateSet], StateGet) => VDom.Element,
      valueF: StateGet => Valid[Value],
  ): PWidget[Action, StateGet, StateSet, Value] =
    PWidget.many[Action, StateGet, StateSet, Value](elementsF(_, _) :: Nil, valueF)

  final class LensBuilder[+Action, +StateGet, -StateSet <: StateGet, +Value](
      widget: PWidget[Action, StateGet, StateSet, Value],
  ) {

    type InnerState >: StateSet <: StateGet

    transparent inline def apply[OuterState](inline lambda: KeywordContext ?=> OuterState => InnerState): Any =
      widget.imapState[OuterState, InnerState](
        GenLens.apply[OuterState].apply[InnerState](lambda).asInstanceOf[Lens[OuterState, InnerState]],
      )

  }

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
