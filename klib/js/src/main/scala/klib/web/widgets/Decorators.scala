package klib.web.widgets

import cats.Monoid
import cats.syntax.either.*
import cats.syntax.monoid.*
import monocle.Lens
import monocle.macros.GenLens

import klib.utils.*
import klib.web.*
import klib.web.VDom.{given, *}
import klib.web.VDomBuilders.{given, *}
import klib.web.widgets.CommonRaises.*

// =====| ... |=====

type CWidgetDecorator = IdFunction.K0[CWidget]
object CWidgetDecorator {
  val identity: CWidgetDecorator = IdFunction.K0.identity
  implicit val monoid: Monoid[CWidgetDecorator] =
    new Monoid[CWidgetDecorator] {
      def empty: CWidgetDecorator = identity
      def combine(x: CWidgetDecorator, y: CWidgetDecorator): CWidgetDecorator =
        w => y(x(w))
    }
}

type PWidgetDecorator = [A, SG, SS <: SG, V] => PWidget[A, SG, SS, V] => PWidget[A, SG, SS, V]
object PWidgetDecorator {
  val identity: PWidgetDecorator = [A, SG, SS <: SG, V] => (w: PWidget[A, SG, SS, V]) => w
  implicit val monoid: Monoid[PWidgetDecorator] =
    new Monoid[PWidgetDecorator] {
      def empty: PWidgetDecorator = identity
      def combine(x: PWidgetDecorator, y: PWidgetDecorator): PWidgetDecorator =
        [A, SG, SS <: SG, V] => (w: PWidget[A, SG, SS, V]) => y[A, SG, SS, V](x[A, SG, SS, V](w))
    }
}

final case class BeforeAfterModifier(
    before: Modifier = (),
    after: Modifier = (),
)
object BeforeAfterModifier {

  def before(modifiers: Modifier*): BeforeAfterModifier = BeforeAfterModifier(before = Modifier.flatten(modifiers.toList))
  def after(modifiers: Modifier*): BeforeAfterModifier = BeforeAfterModifier(after = Modifier.flatten(modifiers.toList))
  def beforeAfter(beforeModifiers: Modifier*)(afterModifiers: Modifier*): BeforeAfterModifier =
    BeforeAfterModifier(
      before = Modifier.flatten(beforeModifiers.toList),
      after = Modifier.flatten(afterModifiers.toList),
    )

  implicit val monoid: Monoid[BeforeAfterModifier] =
    new Monoid[BeforeAfterModifier] {
      def empty: BeforeAfterModifier = BeforeAfterModifier()
      def combine(x: BeforeAfterModifier, y: BeforeAfterModifier): BeforeAfterModifier =
        BeforeAfterModifier(x.before |+| y.before, x.after |+| y.after)
    }

}

final case class BeforeAfterNFModifier(
    before: NameFunction[Modifier] = NameFunction.const[Modifier](()),
    after: NameFunction[Modifier] = NameFunction.const[Modifier](()),
)
object BeforeAfterNFModifier {

  def before(modifiers: Modifier*): BeforeAfterNFModifier = BeforeAfterNFModifier(before = NameFunction.const(Modifier.flatten(modifiers.toList)))
  def after(modifiers: Modifier*): BeforeAfterNFModifier = BeforeAfterNFModifier(after = NameFunction.const(Modifier.flatten(modifiers.toList)))
  def beforeAfter(beforeModifiers: Modifier*)(afterModifiers: Modifier*): BeforeAfterNFModifier =
    BeforeAfterNFModifier(
      before = NameFunction.const(Modifier.flatten(beforeModifiers.toList)),
      after = NameFunction.const(Modifier.flatten(afterModifiers.toList)),
    )

  implicit val monoid: Monoid[BeforeAfterNFModifier] =
    new Monoid[BeforeAfterNFModifier] {
      def empty: BeforeAfterNFModifier = BeforeAfterNFModifier()
      def combine(x: BeforeAfterNFModifier, y: BeforeAfterNFModifier): BeforeAfterNFModifier =
        BeforeAfterNFModifier(x.before |+| y.before, x.after |+| y.after)
    }

}

// =====| Focused |=====

trait FocusableDecorator[Self] {

  def getSelf: Self

  inline final def focus(inline f: Self => List[String]): FocusedClassModifiers[Self] =
    FocusedClassModifiers(getSelf, GenLens[Self](f).asInstanceOf[Lens[Self, List[String]]])

  inline final def focus(inline f: Self => Modifier): FocusedModifier[Self] =
    FocusedModifier(getSelf, GenLens[Self](f).asInstanceOf[Lens[Self, Modifier]])

  inline final def focus(inline f: Self => NameFunction[Modifier]): FocusedNFModifier[Self] =
    FocusedNFModifier(getSelf, GenLens[Self](f).asInstanceOf[Lens[Self, NameFunction[Modifier]]])

  inline final def focus(inline f: Self => CWidgetDecorator): FocusedCWidgetDecorator[Self] =
    FocusedCWidgetDecorator(getSelf, GenLens[Self](f).asInstanceOf[Lens[Self, CWidgetDecorator]])

  inline final def focus(inline f: Self => PWidgetDecorator): FocusedPWidgetDecorator[Self] =
    FocusedPWidgetDecorator(getSelf, GenLens[Self](f).asInstanceOf[Lens[Self, PWidgetDecorator]])

}

final class FocusedClassModifiers[Self](
    self: Self,
    lens: Lens[Self, List[String]],
) {

  def before(other: String*): Self =
    lens.modify(other.toList |+| _)(self)

  def after(other: String*): Self =
    lens.modify(_ |+| other.toList)(self)

}

final class FocusedModifier[Self](
    self: Self,
    lens: Lens[Self, Modifier],
) {

  def |>|(other: Modifier): Self =
    lens.modify(_ |+| other)(self)

  inline def before(other: Modifier*): Self =
    this |>| Modifier.flatten(other.toList)

  inline def |<|(other: Modifier): Self =
    lens.modify(other |+| _)(self)

  inline def after(other: Modifier*): Self =
    this |<| Modifier.flatten(other.toList)

}

final class FocusedNFModifier[Self](
    self: Self,
    lens: Lens[Self, NameFunction[Modifier]],
) {

  def |>|(other: NameFunction[Modifier]): Self =
    lens.modify(_ |>| other)(self)

  inline def before(other: NameFunction[Modifier]): Self =
    this |>| other

  inline def before(other: Modifier*): Self =
    this |>| NameFunction.const[Modifier](other)

  inline def |<|(other: NameFunction[Modifier]): Self =
    lens.modify(_ |<| other)(self)

  inline def after(other: NameFunction[Modifier]): Self =
    this |<| other

  inline def after(other: Modifier*): Self =
    this |<| NameFunction.const[Modifier](other)

}

final class FocusedCWidgetDecorator[Self](
    self: Self,
    lens: Lens[Self, CWidgetDecorator],
) {

  def |>|(other: CWidgetDecorator): Self =
    lens.modify { wF => w => other(wF(w)) }(self)

  inline def before(other: CWidgetDecorator): Self =
    this |>| other

  inline def |<|(other: CWidgetDecorator): Self =
    lens.modify { wF => w => wF(other(w)) }(self)

  inline def after(other: CWidgetDecorator): Self =
    this |<| other

}

final class FocusedPWidgetDecorator[Self](
    self: Self,
    lens: Lens[Self, PWidgetDecorator],
) {

  def |>|(other: PWidgetDecorator): Self =
    lens.modify { PWidgetDecorator.monoid.combine(_, other) }(self)

  inline def before(other: PWidgetDecorator): Self =
    this |>| other

  inline def |<|(other: PWidgetDecorator): Self =
    lens.modify { PWidgetDecorator.monoid.combine(other, _) }(self)

  inline def after(other: PWidgetDecorator): Self =
    this |<| other

}

// =====| Label Decorator |=====

// A helpful way to think of the naming scheme is this:
// `before` means that this decorator decorates before the `other` decorator.
// I think it is easier to reason about when looking at the symbol `|>|`,
// which is why all of the other operations are defined in terms of that.
final case class LabeledFieldDecorator(
    labelClassModifiers: List[String] = Nil,
    labelModifier: BeforeAfterNFModifier = BeforeAfterNFModifier(),
    wrappedModifier: BeforeAfterNFModifier = BeforeAfterNFModifier(),
    decorateLabel: CWidgetDecorator = CWidgetDecorator.identity,
    decorateField: PWidgetDecorator = PWidgetDecorator.identity,
    decorateWrapped: PWidgetDecorator = PWidgetDecorator.identity,
) extends FocusableDecorator[LabeledFieldDecorator] { self =>

  override def getSelf: LabeledFieldDecorator = self

  // =====| merge entire decorator |=====

  def |>|(other: LabeledFieldDecorator): LabeledFieldDecorator =
    LabeledFieldDecorator(
      self.labelClassModifiers |+| other.labelClassModifiers,
      self.labelModifier |+| other.labelModifier,
      self.wrappedModifier |+| other.wrappedModifier,
      CWidgetDecorator.monoid.combine(self.decorateLabel, other.decorateLabel),
      PWidgetDecorator.monoid.combine(self.decorateField, other.decorateField),
      PWidgetDecorator.monoid.combine(self.decorateWrapped, other.decorateWrapped),
    )

  inline def before(other: LabeledFieldDecorator): LabeledFieldDecorator =
    self before other

  inline def |<|(other: LabeledFieldDecorator): LabeledFieldDecorator =
    other before self

  inline def after(other: LabeledFieldDecorator): LabeledFieldDecorator =
    self |<| other

  // =====| nested modifier fields |=====

  def decorate[Action, StateGet, StateSet <: StateGet, Value](
      fieldWidget: PWidget[Action, StateGet, StateSet, Value],
  )(
      name: String,
  ): PWidget[Action, StateGet, StateSet, Value] = {
    val prettyName: String = LabeledFieldDecorator.prettyName(name)
    val labelModifierBefore: Modifier = self.labelModifier.before(name, prettyName)
    val labelModifierAfter: Modifier = self.labelModifier.after(name, prettyName)
    val wrappedModifierBefore: Modifier = self.wrappedModifier.before(name, prettyName)
    val wrappedModifierAfter: Modifier = self.wrappedModifier.after(name, prettyName)

    val baseLabel: CWidget = CWidget(label(labelModifierBefore)(s"$prettyName: ", ClassName.b("widget-label", labelClassModifiers))(labelModifierAfter))
    val modifiedLabel: CWidget = self.decorateLabel(baseLabel)

    val baseField: PWidget[Action, StateGet, StateSet, Value] = fieldWidget.prependErrorString(s"Error in field '$prettyName': ")
    val modifiedField: PWidget[Action, StateGet, StateSet, Value] = self.decorateField(baseField)

    val baseWrapped: PWidget[Action, StateGet, StateSet, Value] = (modifiedLabel >> modifiedField).wrapped(div(wrappedModifierBefore)(_)(wrappedModifierAfter))
    val modifiedWrapped: PWidget[Action, StateGet, StateSet, Value] = self.decorateWrapped(baseWrapped)

    modifiedWrapped
  }

}
object LabeledFieldDecorator {

  val labelInFront: LabeledFieldDecorator = LabeledFieldDecorator()
  val labelAbove: LabeledFieldDecorator = LabeledFieldDecorator(labelModifier = BeforeAfterNFModifier(after = NameFunction.const(display := "block")))

  def prettyName(name: String): String = name.split("-").map(_.capitalize).mkString(" ")

}
