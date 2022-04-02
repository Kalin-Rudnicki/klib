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

// =====| ... |=====

final class FocusedNameFunctionModifier private[widgets] (
    self: LabeledFieldDecorator,
    lens: Lens[LabeledFieldDecorator, NameFunction[Modifier]],
) {

  def |>|(other: NameFunction[Modifier]): LabeledFieldDecorator =
    lens.modify(_ |>| other)(self)

  inline def before(other: NameFunction[Modifier]): LabeledFieldDecorator =
    this |>| other

  inline def before(other: Modifier*): LabeledFieldDecorator =
    this |>| NameFunction.const[Modifier](other)

  inline def |<|(other: NameFunction[Modifier]): LabeledFieldDecorator =
    lens.modify(_ |<| other)(self)

  inline def after(other: NameFunction[Modifier]): LabeledFieldDecorator =
    this |<| other

  inline def after(other: Modifier*): LabeledFieldDecorator =
    this |<| NameFunction.const[Modifier](other)

}

final class FocusedCWidgetDecorator private[widgets] (
    self: LabeledFieldDecorator,
    lens: Lens[LabeledFieldDecorator, CWidgetDecorator],
) {

  def |>|(other: CWidgetDecorator): LabeledFieldDecorator =
    lens.modify { wF => w => other(wF(w)) }(self)

  inline def before(other: CWidgetDecorator): LabeledFieldDecorator =
    this |>| other

  inline def |<|(other: CWidgetDecorator): LabeledFieldDecorator =
    lens.modify { wF => w => wF(other(w)) }(self)

  inline def after(other: CWidgetDecorator): LabeledFieldDecorator =
    this |<| other

}

final class FocusedPWidgetDecorator private[widgets] (
    self: LabeledFieldDecorator,
    lens: Lens[LabeledFieldDecorator, PWidgetDecorator],
) {

  def |>|(other: PWidgetDecorator): LabeledFieldDecorator =
    lens.modify { PWidgetDecorator.monoid.combine(_, other) }(self)

  inline def before(other: PWidgetDecorator): LabeledFieldDecorator =
    this |>| other

  inline def |<|(other: PWidgetDecorator): LabeledFieldDecorator =
    lens.modify { PWidgetDecorator.monoid.combine(other, _) }(self)

  inline def after(other: PWidgetDecorator): LabeledFieldDecorator =
    this |<| other

}

// =====| ... |=====

// A helpful way to think of the naming scheme is this:
// `before` means that this decorator decorates before the `other` decorator.
// I think it is easier to reason about when looking at the symbol `|>|`,
// which is why all of the other operations are defined in terms of that.
final case class LabeledFieldDecorator(
    labelModifierBefore: NameFunction[Modifier] = NameFunction.const(()),
    labelModifierAfter: NameFunction[Modifier] = NameFunction.const(()),
    wrappedModifierBefore: NameFunction[Modifier] = NameFunction.const(()),
    wrappedModifierAfter: NameFunction[Modifier] = NameFunction.const(()),
    decorateLabel: CWidgetDecorator = CWidgetDecorator.identity,
    decorateField: PWidgetDecorator = PWidgetDecorator.identity,
    decorateWrapped: PWidgetDecorator = PWidgetDecorator.identity,
) { self =>

  // =====| merge entire decorator |=====

  def |>|(other: LabeledFieldDecorator): LabeledFieldDecorator =
    LabeledFieldDecorator(
      self.labelModifierBefore |+| other.labelModifierBefore,
      self.labelModifierAfter |+| other.labelModifierAfter,
      self.wrappedModifierBefore |+| other.wrappedModifierBefore,
      self.wrappedModifierAfter |+| other.wrappedModifierAfter,
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

  // --- generic focuses ---

  def focusNamedFunctionModifier(
      lens: Lens[LabeledFieldDecorator, NameFunction[Modifier]],
  ): FocusedNameFunctionModifier =
    FocusedNameFunctionModifier(self, lens)

  def focusCWidgetDecorator(
      lens: Lens[LabeledFieldDecorator, CWidgetDecorator],
  ): FocusedCWidgetDecorator =
    FocusedCWidgetDecorator(self, lens)

  def focusPWidgetDecorator(
      lens: Lens[LabeledFieldDecorator, PWidgetDecorator],
  ): FocusedPWidgetDecorator =
    FocusedPWidgetDecorator(self, lens)

  // --- helper focuses ---

  def focusLabelModifierBefore: FocusedNameFunctionModifier =
    focusNamedFunctionModifier(GenLens[LabeledFieldDecorator](_.labelModifierBefore))

  def focusLabelModifierAfter: FocusedNameFunctionModifier =
    focusNamedFunctionModifier(GenLens[LabeledFieldDecorator](_.labelModifierAfter))

  def focusWrappedModifierBefore: FocusedNameFunctionModifier =
    focusNamedFunctionModifier(GenLens[LabeledFieldDecorator](_.wrappedModifierBefore))

  def focusWrappedModifierAfter: FocusedNameFunctionModifier =
    focusNamedFunctionModifier(GenLens[LabeledFieldDecorator](_.wrappedModifierAfter))

  def focusDecorateLabel: FocusedCWidgetDecorator =
    focusCWidgetDecorator(GenLens[LabeledFieldDecorator](_.decorateLabel))

  def focusDecorateField: FocusedPWidgetDecorator =
    focusPWidgetDecorator(GenLens[LabeledFieldDecorator](_.decorateField))

  def focusDecorateWrapped: FocusedPWidgetDecorator =
    focusPWidgetDecorator(GenLens[LabeledFieldDecorator](_.decorateWrapped))

  // =====| nested modifier fields |=====

  def decorate[Action, StateGet, StateSet <: StateGet, Value](
      fieldWidget: PWidget[Action, StateGet, StateSet, Value],
  )(
      name: String,
  ): PWidget[Action, StateGet, StateSet, Value] = {
    val prettyName: String = LabeledFieldDecorator.prettyName(name)
    val labelModifierBefore: Modifier = self.labelModifierBefore(name, prettyName)
    val labelModifierAfter: Modifier = self.labelModifierAfter(name, prettyName)
    val wrappedModifierBefore: Modifier = self.wrappedModifierBefore(name, prettyName)
    val wrappedModifierAfter: Modifier = self.wrappedModifierAfter(name, prettyName)

    val baseLabel: CWidget = CWidget(label(labelModifierBefore)(s"$prettyName: ")(labelModifierAfter))
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
  val labelAbove: LabeledFieldDecorator = LabeledFieldDecorator(labelModifierAfter = NameFunction.const(display := "block"))

  def prettyName(name: String): String = name.split("-").map(_.capitalize).mkString(" ")

}
