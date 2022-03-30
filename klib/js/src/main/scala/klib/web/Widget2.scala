package klib.web

import cats.data.*
import cats.syntax.either.*
import cats.syntax.parallel.*
import zio.Zippable

object tmp {

  type Valid[T] = EitherNel[String, T]

  sealed trait Modifier[+Action, -StateGet, +StateSet <: StateGet] {
    def basics: List[Modifier.Basic[Action, StateGet, StateSet]]
  }
  object Modifier {
    sealed trait Basic[+Action, -StateGet, +StateSet <: StateGet] extends Modifier[Action, StateGet, StateSet] {
      override def basics: List[Modifier.Basic[Action, StateGet, StateSet]] = this :: Nil
    }
    final case class Multi[+Action, -StateGet, +StateSet <: StateGet](children: List[Modifier.Basic[Action, StateGet, StateSet]]) extends Modifier[Action, StateGet, StateSet] {
      override def basics: List[Modifier.Basic[Action, StateGet, StateSet]] = children
    }

    def apply[Action, StateGet, StateSet <: StateGet](modifiers: Modifier[Action, StateGet, StateSet]*): Modifier[Action, StateGet, StateSet] =
      Multi[Action, StateGet, StateSet](modifiers.toList.flatMap(_.basics))

    def join[Action, StateGet, StateSet <: StateGet](
        current: Modifier[Action, StateGet, StateSet],
        other: Seq[Modifier[Action, StateGet, StateSet]],
    ): Modifier[Action, StateGet, StateSet] =
      Multi[Action, StateGet, StateSet]((current :: other.toList).flatMap(_.basics))

  }

  sealed trait Attribute[+Action, -StateGet, +StateSet <: StateGet] extends Modifier[Action, StateGet, StateSet]

  final case class ModifierSRH[+Action, -StateGet, +StateSet <: StateGet](
      child: (StateGet, RaiseHandler[Action, StateSet]) => Modifier[Action, StateGet, StateSet],
  ) extends Modifier.Basic[Action, StateGet, StateSet] {
    override def basics: List[Modifier.Basic[Action, StateGet, StateSet]] = this :: Nil
  }

  sealed trait PWidget[+Action, -StateGet, +StateSet <: StateGet, +Value] extends Modifier.Basic[Action, StateGet, StateSet] { self =>

    // =====| Abstract |=====

    type SelfT[+A, -SG, +SS <: SG, +V]

    def value(state: StateGet): Valid[Value]

    protected def copySelfWithNewValueF[NewStateGet >: StateSet <: StateGet, NewValue](newValueF: NewStateGet => Valid[NewValue]): SelfT[Action, NewStateGet, StateSet, NewValue]

    // =====| Combinators |=====

    final def >>[NewAction >: Action, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet, Value2](
        other: PWidget[NewAction, NewStateGet, NewStateSet, Value2],
    )(implicit
        zip: Zippable[Value, Value2],
    ): PWidget[NewAction, NewStateGet, NewStateSet, zip.Out] =
      ElementsWidget[NewAction, NewStateGet, NewStateSet, zip.Out](
        IArray(self, other),
        state => (self.value(state), other.value(state)).parMapN(zip.zip),
      )

    inline final def thisThenOther[NewAction >: Action, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet, Value2](
        other: PWidget[NewAction, NewStateGet, NewStateSet, Value2],
    )(implicit
        zip: Zippable[Value, Value2],
    ): PWidget[NewAction, NewStateGet, NewStateSet, zip.Out] =
      self.>>[NewAction, NewStateGet, NewStateSet, Value2](other)

    final def <<[NewAction >: Action, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet, Value2](
        other: PWidget[NewAction, NewStateGet, NewStateSet, Value2],
    )(implicit
        zip: Zippable[Value2, Value],
    ): PWidget[NewAction, NewStateGet, NewStateSet, zip.Out] =
      ElementsWidget[NewAction, NewStateGet, NewStateSet, zip.Out](
        IArray(other, self),
        state => (other.value(state), self.value(state)).parMapN(zip.zip),
      )

    inline final def otherThenThis[NewAction >: Action, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet, Value2](
        other: PWidget[NewAction, NewStateGet, NewStateSet, Value2],
    )(implicit
        zip: Zippable[Value2, Value],
    ): PWidget[NewAction, NewStateGet, NewStateSet, zip.Out] =
      self.<<[NewAction, NewStateGet, NewStateSet, Value2](other)

    // =====| Value Mapping |=====

    inline final def mapValueSVE[NewStateGet >: StateSet <: StateGet, Value2](f: (NewStateGet, Valid[Value]) => Valid[Value2]): SelfT[Action, NewStateGet, StateSet, Value2] =
      copySelfWithNewValueF(state => f(state, value(state)))

    inline final def mapValueSVF[NewStateGet >: StateSet <: StateGet, Value2](f: (NewStateGet, Value) => Valid[Value2]): SelfT[Action, NewStateGet, StateSet, Value2] =
      copySelfWithNewValueF(state => value(state).flatMap(f(state, _)))

    inline final def mapValueSV[NewStateGet >: StateSet <: StateGet, Value2](f: (NewStateGet, Value) => Value2): SelfT[Action, NewStateGet, StateSet, Value2] =
      copySelfWithNewValueF(state => value(state).map(f(state, _)))

    inline final def setValueSE[NewStateGet >: StateSet <: StateGet, Value2](f: NewStateGet => Valid[Value2]): SelfT[Action, NewStateGet, StateSet, Value2] =
      copySelfWithNewValueF(f)

    inline final def setValueS[NewStateGet >: StateSet <: StateGet, Value2](f: NewStateGet => Value2): SelfT[Action, NewStateGet, StateSet, Value2] =
      copySelfWithNewValueF(f(_).asRight)

    inline final def mapValueVE[Value2](f: Valid[Value] => Valid[Value2]): SelfT[Action, StateGet, StateSet, Value2] =
      copySelfWithNewValueF(state => f(value(state)))

    inline final def mapValueVF[Value2](f: Value => Valid[Value2]): SelfT[Action, StateGet, StateSet, Value2] =
      copySelfWithNewValueF(value(_).flatMap(f))

    inline final def mapValueV[Value2](f: Value => Value2): SelfT[Action, StateGet, StateSet, Value2] =
      copySelfWithNewValueF(value(_).map(f))

    inline final def setValueE[Value2](f: => Valid[Value2]): SelfT[Action, StateGet, StateSet, Value2] =
      copySelfWithNewValueF(_ => f)

    inline final def setValue[Value2](f: => Value2): SelfT[Action, StateGet, StateSet, Value2] =
      copySelfWithNewValueF(_ => f.asRight)

    // =====| Aliases |=====

    inline final def asEither[Value2](f: => Valid[Value2]): SelfT[Action, StateGet, StateSet, Value2] =
      setValueE(f)

    inline final def as[Value2](f: => Value2): SelfT[Action, StateGet, StateSet, Value2] =
      setValueE(f.asRight)

    inline final def asError(f: => NonEmptyList[String]): SelfT[Action, StateGet, StateSet, Nothing] =
      setValueE(f.asLeft)

    // =====| Misc |=====

    inline final def either: SelfT[Action, StateGet, StateSet, Valid[Value]] =
      copySelfWithNewValueF(value(_).asRight)

  }

  type CWidget = PWidget[Nothing, Any, Nothing, Unit]
  type CAWidget[+Action] = PWidget[Action, Any, Nothing, Unit]
  type CVWidget[+Value] = PWidget[Nothing, Any, Nothing, Value]
  type CAVWidget[+Action, +Value] = PWidget[Action, Any, Nothing, Value]
  type Widget[State] = PWidget[Nothing, State, State, Unit]
  type AWidget[+Action, State] = PWidget[Action, State, State, Unit]
  type VWidget[State, +Value] = PWidget[Nothing, State, State, Value]
  type AVWidget[+Action, State, +Value] = PWidget[Action, State, State, Value]

  final case class TextWidget[-StateGet, +Value](
      text: String,
      valueF: StateGet => Valid[Value],
  ) extends PWidget[Nothing, StateGet, Nothing, Value] {

    override type SelfT[+A, -SG, +SS <: SG, +V] = TextWidget[SG, V]

    def value(state: StateGet): Valid[Value] = valueF(state)

    override protected def copySelfWithNewValueF[NewStateGet >: Nothing <: StateGet, NewValue](newValueF: NewStateGet => Valid[NewValue]): TextWidget[NewStateGet, NewValue] =
      TextWidget(text, newValueF)

  }

  final case class ElementWidget[+Action, -StateGet, +StateSet <: StateGet, +Value](
      tagName: String,
      modifier: Modifier[Action, StateGet, StateSet],
      valueF: StateGet => Valid[Value],
  ) extends PWidget[Action, StateGet, StateSet, Value] {

    override type SelfT[+A, -SG, +SS <: SG, +V] = ElementWidget[A, SG, SS, V]

    override def value(state: StateGet): Valid[Value] = valueF(state)

    override protected def copySelfWithNewValueF[NewStateGet >: StateSet <: StateGet, Value2](f: NewStateGet => Valid[Value2]): SelfT[Action, NewStateGet, StateSet, Value2] =
      ElementWidget[Action, NewStateGet, StateSet, Value2](tagName, modifier, f)

    def includeModifiers[NewAction >: Action, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet](
        modifiers: Modifier[NewAction, NewStateGet, NewStateSet]*,
    ): ElementWidget[NewAction, NewStateGet, NewStateSet, Value] =
      ElementWidget(tagName, Modifier.join[NewAction, NewStateGet, NewStateSet](modifier, modifiers), valueF)

    def apply[NewAction >: Action, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet](
        modifiers: Modifier[NewAction, NewStateGet, NewStateSet]*,
    ): ElementWidget[NewAction, NewStateGet, NewStateSet, Value] =
      includeModifiers[NewAction, NewStateGet, NewStateSet](modifiers*)

    def includeWidgetUseValue[NewAction >: Action, NewStateGet <: StateGet, NewStateSet >: StateSet <: NewStateGet, Value2](
        widget: PWidget[NewAction, NewStateGet, NewStateSet, Value2],
    ): ElementWidget[NewAction, NewStateGet, NewStateSet, Value2] =
      ElementWidget(tagName, Modifier[NewAction, NewStateGet, NewStateSet](modifier, widget), widget.value)

  }
  final case class ElementsWidget[+Action, -StateGet, +StateSet <: StateGet, +Value](
      elements: IArray[PWidget[Action, StateGet, StateSet, Any]],
      valueF: StateGet => Valid[Value],
  ) extends PWidget[Action, StateGet, StateSet, Value] {

    override type SelfT[+A, -SG, +SS <: SG, +V] = ElementsWidget[A, SG, SS, V]

    override def value(state: StateGet): Valid[Value] = valueF(state)

    override protected def copySelfWithNewValueF[NewStateGet >: StateSet <: StateGet, Value2](f: NewStateGet => Valid[Value2]): SelfT[Action, NewStateGet, StateSet, Value2] =
      ElementsWidget[Action, NewStateGet, StateSet, Value2](elements, f)

  }

}
