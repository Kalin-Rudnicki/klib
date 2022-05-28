package klib.web

import cats.data.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import monocle.*
import monocle.Focus.KeywordContext
import monocle.macros.GenLens
import scala.annotation.targetName
import scala.annotation.unchecked.uncheckedVariance
import scala.quoted.*
import zio.*
import zio.json.*

import klib.utils.*
import klib.web.VDomBuilders.CSSAttrBuilders.display
import klib.web.widgets.LabeledFieldDecorator

trait PWidget[+Action, -StateGet, +StateSet <: StateGet, +Value] { self =>

  // =====| Helpers |=====

  val elements: (RaiseHandler[Action, StateSet], StateGet) => List[VDom.Element]
  val value: StateGet => Valid[Value]

  // =====| Combinators |=====

  // --- place before/after ---

  final def placeBefore[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    PWidget.many(
      (rh, s) => self.elements(rh, s) ::: other.elements(rh, s),
      s => (self.value(s), other.value(s)).parMapN(zippable.zip),
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
      s => (other.value(s), self.value(s)).parMapN(zippable.zip),
    )
  inline final def <<[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value2, Value],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    placeAfter[Action2, StateGet2, StateSet2, Value2](other)

  // --- place before/after with value ---

  final def placeBeforeWithEitherValue[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: Valid[Value] => PWidget[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zippable: Zippable[Value2, Value],
  ): PWidget[Action2, StateGet2, StateSet2, zippable.Out] =
    PWidget.many(
      (rh, s) => other(value(s)).elements(rh, s) ::: self.elements(rh, s),
      { s =>
        val svs = self.value(s)
        (other(svs).value(s), svs).parMapN(zippable.zip)
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
        (svs, other(svs).value(s)).parMapN(zippable.zip)
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

  final def genericDebugState[NewStateGet >: StateSet <: StateGet](showState: NewStateGet => String): PWidget[Action, NewStateGet, StateSet, Value] =
    self >>
      PWidget[Action, NewStateGet, StateSet, Unit](
        (_, s) =>
          VDomBuilders.div(
            VDomBuilders.span(
              "State: ",
              VDomBuilders.display.inlineBlock,
              VDomBuilders.height := "100%",
              VDomBuilders.verticalAlign := "top",
              VDomBuilders.paddingRight := "15px",
            ),
            VDomBuilders.span(showState(s), VDomBuilders.display.inlineBlock, VDomBuilders.whiteSpace.pre),
          ),
        _ => ().asRight,
      )

  final def genericDebugValue[NewValue >: Value](showValue: NewValue => String): PWidget[Action, StateGet, StateSet, NewValue] =
    placeAfterWithEitherValue { e =>
      CWidget(
        VDomBuilders.div(
          VDomBuilders.span(
            "Value: ",
            VDomBuilders.display.inlineBlock,
            VDomBuilders.height := "100%",
            VDomBuilders.verticalAlign := "top",
            VDomBuilders.paddingRight := "15px",
          ),
          VDomBuilders.span(
            e match {
              case Right(v) => showValue(v)
              case Left(e)  => s"Errors:${e.toList.map(e => s"\n  - $e").mkString}"
            },
            VDomBuilders.display.inlineBlock,
            VDomBuilders.whiteSpace.pre,
          ),
        ),
      )
    }

  inline final def debugState: PWidget[Action, StateGet, StateSet, Value] =
    genericDebugState[StateGet](_.toString)

  inline final def debugValue: PWidget[Action, StateGet, StateSet, Value] =
    genericDebugValue[Value](_.toString)

  inline final def debugStateAndValue: PWidget[Action, StateGet, StateSet, Value] =
    self.debugState.debugValue

  inline final def debugStateJson[NewStateGet >: StateSet <: StateGet](implicit
      stateEncoder: JsonEncoder[NewStateGet],
  ): PWidget[Action, NewStateGet, StateSet, Value] =
    genericDebugState[NewStateGet](_.toJsonPretty)

  inline final def debugValueJson[NewValue >: Value](implicit
      valueEncoder: JsonEncoder[NewValue],
  ): PWidget[Action, StateGet, StateSet, NewValue] =
    genericDebugValue[NewValue](_.toJsonPretty)

  inline final def debugStateAndValueJson[NewStateGet >: StateSet <: StateGet, NewValue >: Value](implicit
      stateEncoder: JsonEncoder[NewStateGet],
      valueEncoder: JsonEncoder[NewValue],
  ): PWidget[Action, NewStateGet, StateSet, NewValue] =
    self.debugStateJson[NewStateGet].debugValueJson[NewValue]

  // --- wrap ---

  @targetName("wrappedMany")
  final def wrapped(wrapInner: List[VDom.Element] => List[VDom.Element]): PWidget[Action, StateGet, StateSet, Value] =
    PWidget.many(
      (rh, s) => wrapInner(elements(rh, s)),
      value,
    )

  inline final def wrapped(wrapInner: List[VDom.Element] => VDom.Element): PWidget[Action, StateGet, StateSet, Value] =
    wrapped(wrapInner(_) :: Nil)

  // =====| Value Mapping |=====

  def copySelfWithNewValueF[NewStateGet >: StateSet <: StateGet, NewValue](newValueF: NewStateGet => Valid[NewValue]): PWidget[Action, NewStateGet, StateSet, NewValue] =
    PWidget.many[Action, NewStateGet, StateSet, NewValue](
      elements,
      newValueF,
    )

  inline final def mapValueSVE[NewStateGet >: StateSet <: StateGet, Value2](f: (NewStateGet, Valid[Value]) => Valid[Value2]): PWidget[Action, NewStateGet, StateSet, Value2] =
    copySelfWithNewValueF(state => f(state, value(state)))

  inline final def mapValueSVF[NewStateGet >: StateSet <: StateGet, Value2](f: (NewStateGet, Value) => Valid[Value2]): PWidget[Action, NewStateGet, StateSet, Value2] =
    copySelfWithNewValueF(state => value(state).flatMap(f(state, _)))

  inline final def mapValueSV[NewStateGet >: StateSet <: StateGet, Value2](f: (NewStateGet, Value) => Value2): PWidget[Action, NewStateGet, StateSet, Value2] =
    copySelfWithNewValueF(state => value(state).map(f(state, _)))

  inline final def setValueSE[NewStateGet >: StateSet <: StateGet, Value2](f: NewStateGet => Valid[Value2]): PWidget[Action, NewStateGet, StateSet, Value2] =
    copySelfWithNewValueF(f)

  inline final def setValueS[NewStateGet >: StateSet <: StateGet, Value2](f: NewStateGet => Value2): PWidget[Action, NewStateGet, StateSet, Value2] =
    copySelfWithNewValueF(f(_).asRight)

  inline final def mapValueVE[Value2](f: Valid[Value] => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    copySelfWithNewValueF(state => f(value(state)))

  inline final def mapValueVF[Value2](f: Value => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    copySelfWithNewValueF(value(_).flatMap(f))

  inline final def mapValueV[Value2](f: Value => Value2): PWidget[Action, StateGet, StateSet, Value2] =
    copySelfWithNewValueF(value(_).map(f))

  inline final def setValueE[Value2](f: => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    copySelfWithNewValueF(_ => f)

  inline final def setValue[Value2](f: => Value2): PWidget[Action, StateGet, StateSet, Value2] =
    copySelfWithNewValueF(_ => f.asRight)

  // --- Aliases ---

  inline final def asEither[Value2](f: => Valid[Value2]): PWidget[Action, StateGet, StateSet, Value2] =
    setValueE(f)

  inline final def as[Value2](f: => Value2): PWidget[Action, StateGet, StateSet, Value2] =
    setValueE(f.asRight)

  inline final def asError(e: => NonEmptyList[String]): PWidget[Action, StateGet, StateSet, Nothing] =
    setValueE(e.asLeft)

  inline final def asError(e0: => String, eN: => String*): PWidget[Action, StateGet, StateSet, Nothing] =
    asError(NonEmptyList(e0, eN.toList))

  inline final def mapErrorString(f: String => String): PWidget[Action, StateGet, StateSet, Value] =
    mapValueVE(_.leftMap(_.map(f)))

  inline final def prependErrorString(s: => String): PWidget[Action, StateGet, StateSet, Value] =
    mapErrorString(e => s"$s$e")

  // =====| Map State |=====

  final def imapState[OuterState, InnerState >: StateSet <: StateGet](
      lens: Lens[OuterState, InnerState],
  ): AVWidget[Action, OuterState, Value] =
    PWidget.many(
      (rh, s) => elements(rh.mapState(lens), lens.get(s)),
      s => value(lens.get(s)),
    )

  inline final def zoomOut[OuterState](
      inline f: OuterState => StateGet,
  ): AVWidget[Action, OuterState, Value] =
    imapState[OuterState, StateGet](GenLens[OuterState](f).asInstanceOf[Lens[OuterState, StateGet]])

  // =====| Map Action |=====

  final def mapActionASVE[NewAction, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet](
      f: (Action, NewStateGet, Valid[Value]) => SKTask[List[Raise[NewAction, NewStateSet]]],
  ): PWidget[NewAction, NewStateGet, NewStateSet, Value] =
    PWidget.many[NewAction, NewStateGet, NewStateSet, Value](
      (rh, s) =>
        elements(
          rh.mapAction { a => f(a, s, value(s)) },
          s,
        ),
      value,
    )

  inline final def mapActionASV[NewAction, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet](
      f: (Action, NewStateGet, Value) => SKTask[List[Raise[NewAction, NewStateSet]]],
  ): PWidget[NewAction, NewStateGet, NewStateSet, Value] =
    mapActionASVE[NewAction, NewStateGet, NewStateSet] { (a, s, v) =>
      v match {
        case Right(v) => f(a, s, v)
        case Left(e)  => ZIO.fail(e.map(KError.UserError(_))) // TODO: Should probably do something more like "DisplayMessage"
      }
    }

  inline final def mapActionAS[NewAction, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet](
      f: (Action, NewStateGet) => SKTask[List[Raise[NewAction, NewStateSet]]],
  ): PWidget[NewAction, NewStateGet, NewStateSet, Value] =
    mapActionASVE[NewAction, NewStateGet, NewStateSet] { (a, s, _) => f(a, s) }

  inline final def mapActionAVE[NewAction, NewStateSet >: StateSet <: StateGet](
      f: (Action, Valid[Value]) => SKTask[List[Raise[NewAction, NewStateSet]]],
  ): PWidget[NewAction, StateGet, NewStateSet, Value] =
    mapActionASVE[NewAction, StateGet, NewStateSet] { (a, _, v) => f(a, v) }

  inline final def mapActionAV[NewAction, NewStateSet >: StateSet <: StateGet](
      f: (Action, Value) => SKTask[List[Raise[NewAction, NewStateSet]]],
  ): PWidget[NewAction, StateGet, NewStateSet, Value] =
    mapActionASV[NewAction, StateGet, NewStateSet] { (a, _, v) => f(a, v) }

  inline final def mapActionA[NewAction, NewStateSet >: StateSet <: StateGet](
      f: Action => SKTask[List[Raise[NewAction, NewStateSet]]],
  ): PWidget[NewAction, StateGet, NewStateSet, Value] =
    mapActionASVE[NewAction, StateGet, NewStateSet] { (a, _, _) => f(a) }

  // =====| Form Helpers |=====

  final def required[V2](implicit ev: Value <:< Option[V2]): PWidget[Action, StateGet, StateSet, V2] =
    mapValueVF { v =>
      ev(v) match {
        case Some(v2) => v2.asRight
        case None     => NonEmptyList.one("Missing required").asLeft
      }
    }

  // NOTE : Call this after doing any value validations.
  //      : This way, the error message will include the label.
  final def labeled(name: String, decorator: LabeledFieldDecorator): PWidget[Action, StateGet, StateSet, Value] =
    decorator.decorate(self)(name)

  inline final def labeledInFront(
      name: String,
      decorator: LabeledFieldDecorator => LabeledFieldDecorator = identity,
  ): PWidget[Action, StateGet, StateSet, Value] =
    labeled(name, decorator(LabeledFieldDecorator.labelInFront))

  inline final def labeledAbove(
      name: String,
      decorator: LabeledFieldDecorator => LabeledFieldDecorator = identity,
  ): PWidget[Action, StateGet, StateSet, Value] =
    labeled(name, decorator(LabeledFieldDecorator.labelAbove))

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

  final class Builder1[Action] {
    def withState[State]: Builder2[Action, State, State] = new Builder2[Action, State, State]
    def withNoState: Builder2[Action, Any, Nothing] = new Builder2[Action, Any, Nothing]
  }
  final class Builder2[Action, StateGet, StateSet <: StateGet] {
    def withValueF[Value](f: StateGet => Valid[Value]): Builder3[Action, StateGet, StateSet, Value] = new Builder3(f)
    inline def withConstValue[Value](f: => Valid[Value]): Builder3[Action, StateGet, StateSet, Value] = withValueF(_ => f)
    inline def withNoValue: Builder3[Action, StateGet, StateSet, Unit] = withValueF(_ => ().asRight)
  }
  final class Builder3[Action, StateGet, StateSet <: StateGet, Value](valueF: StateGet => Valid[Value]) {

    def withElementRS(
        f: (RaiseHandler[Action, StateSet], StateGet) => VDom.Element,
    ): PWidget[Action, StateGet, StateSet, Value] =
      PWidget[Action, StateGet, StateSet, Value](f(_, _), valueF)

    inline def withElementR(
        f: RaiseHandler[Action, StateSet] => VDom.Element,
    ): PWidget[Action, StateGet, StateSet, Value] =
      withElementRS((rh, _) => f(rh))

    inline def withElementS(
        f: StateGet => VDom.Element,
    ): PWidget[Action, StateGet, StateSet, Value] =
      withElementRS((_, s) => f(s))

    inline def withElement(
        f: => VDom.Element,
    ): PWidget[Action, StateGet, StateSet, Value] =
      withElementRS((_, _) => f)

    def withElementsRS(
        f: (RaiseHandler[Action, StateSet], StateGet) => List[VDom.Element],
    ): PWidget[Action, StateGet, StateSet, Value] =
      PWidget.many[Action, StateGet, StateSet, Value](f(_, _), valueF)

    inline def withElementsR(
        f: RaiseHandler[Action, StateSet] => List[VDom.Element],
    ): PWidget[Action, StateGet, StateSet, Value] =
      withElementsRS((rh, _) => f(rh))

    inline def withElementsS(
        f: StateGet => List[VDom.Element],
    ): PWidget[Action, StateGet, StateSet, Value] =
      withElementsRS((_, s) => f(s))

    inline def withElements(
        f: => List[VDom.Element],
    ): PWidget[Action, StateGet, StateSet, Value] =
      withElementsRS((_, _) => f)

    @targetName("withElements*")
    inline def withElements(
        f: => VDom.Element*,
    ): PWidget[Action, StateGet, StateSet, Value] =
      withElementsRS((_, _) => f.toList)

  }

  def withAction[Action]: Builder1[Action] = new Builder1[Action]
  inline def withNoAction: Builder1[Nothing] = withAction[Nothing]

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
      elementsF: => VDom.Element*,
  ): CWidget =
    CWidget.many(elementsF.toList)

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
