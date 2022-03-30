package klib.web.widgets

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import monocle.Lens
import monocle.macros.GenLens
import org.scalajs.dom.console
import scala.scalajs.js.Dynamic

import klib.fp.typeclass.DecodeFromString
import klib.utils.*
import klib.web.*
export klib.web.VDom.{given, *}
export klib.web.VDomBuilders.{given, *}
export klib.web.widgets.CommonRaises.*

object TextWidgets {

  sealed trait TextBuilder1 { self =>

    type Decorator

    protected def defaultDecorator: Decorator

    protected def generic[T: DecodeFromString, O](
        baseElement: => NodeElement,
    )(
        tToO: Option[T] => Valid[O],
    )(
        name: String,
        dec: Decorator,
    ): AVWidget[Submit, String, O]

    final class TextBuilder2 private[TextBuilder1] (baseElement: => NodeElement) {

      def required[T: DecodeFromString](
          name: String,
          decorator: Decorator = defaultDecorator,
      ): AVWidget[Submit, String, T] =
        generic[T, T](baseElement) {
          case Some(t) => t.asRight
          case None    => KError.message.same("Missing required field").asLeft
        }(name, decorator)

      def optional[T: DecodeFromString](
          name: String,
          decorator: Decorator = defaultDecorator,
      ): AVWidget[Submit, String, Option[T]] =
        generic[T, Option[T]](baseElement)(_.asRight)(name, decorator)

    }

    def input: TextBuilder2 = TextBuilder2(VDomBuilders.input)
    def textArea: TextBuilder2 = TextBuilder2(VDomBuilders.textarea)

  }

  object basic extends TextBuilder1 {

    // A helpful way to think of the naming scheme is this:
    // `before` means that this decorator decorates before the `other` decorator.
    // I think it is easier to reason about when looking at the symbol `|>|`,
    // which is why all of the other operations are defined in terms of that.
    final case class Decorator(
        labelModifier: NameFunction[Modifier] = NameFunction.const(()),
        fieldModifier: NameFunction[Modifier] = NameFunction.const(()),
        wrappedModifier: NameFunction[Modifier] = NameFunction.const(()),
        decorateLabel: IdFunction.K0[CWidget] = IdFunction.K0.identity,
        decorateField: IdFunction.K2[AVSubmit] = IdFunction.K2.identity,
        decorateWrapped: IdFunction.K2[AVSubmit] = IdFunction.K2.identity,
    ) { self =>

      // =====| nested modifier builders |=====

      final class NameFunctionModifier private[Decorator] (lens: Lens[Decorator, NameFunction[Modifier]]) {

        def |>|(other: NameFunction[Modifier]): Decorator =
          lens.modify(_ |>| other)(self)

        inline def before(other: NameFunction[Modifier]): Decorator =
          this |>| other

        inline def before(other: Modifier*): Decorator =
          this |>| NameFunction.const[Modifier](other)

        inline def |<|(other: NameFunction[Modifier]): Decorator =
          lens.modify(_ |<| other)(self)

        inline def after(other: NameFunction[Modifier]): Decorator =
          this |<| other

        inline def after(other: Modifier*): Decorator =
          this |<| NameFunction.const[Modifier](other)

      }

      final class IdFunctionK0CWidgetModifier(lens: Lens[Decorator, IdFunction.K0[CWidget]]) {

        def |>|(other: IdFunction.K0[CWidget]): Decorator =
          lens.modify { wF => w => other(wF(w)) }(self)

        inline def before(other: IdFunction.K0[CWidget]): Decorator =
          this |>| other

        inline def |<|(other: IdFunction.K0[CWidget]): Decorator =
          lens.modify { wF => w => wF(other(w)) }(self)

        inline def after(other: IdFunction.K0[CWidget]): Decorator =
          this |<| other

      }

      final class IdFunctionK2AVSubmitModifier(lens: Lens[Decorator, IdFunction.K2[AVSubmit]]) {

        def |>|(other: IdFunction.K2[AVSubmit]): Decorator =
          lens.modify { (wF: IdFunction.K2[AVSubmit]) => [T, O] => (w: AVSubmit[T, O]) => other(wF(w)) }(self)

        inline def before(other: IdFunction.K2[AVSubmit]): Decorator =
          this |>| other

        inline def |<|(other: IdFunction.K2[AVSubmit]): Decorator =
          lens.modify { (wF: IdFunction.K2[AVSubmit]) => [T, O] => (w: AVSubmit[T, O]) => wF(other(w)) }(self)

        inline def after(other: IdFunction.K2[AVSubmit]): Decorator =
          this |<| other

      }

      // =====| merge entire decorator |=====

      def |>|(other: Decorator): Decorator =
        Decorator(
          self.labelModifier |>| other.labelModifier,
          self.fieldModifier |>| other.fieldModifier,
          self.wrappedModifier |>| other.wrappedModifier,
          w => other.decorateLabel(self.decorateLabel(w)),
          [T, O] => (w: AVWidget[Submit, T, O]) => other.decorateField[T, O](self.decorateField[T, O](w)),
          [T, O] => (w: AVWidget[Submit, T, O]) => other.decorateWrapped[T, O](self.decorateWrapped[T, O](w)),
        )

      inline def before(other: Decorator): Decorator =
        self before other

      inline def |<|(other: Decorator): Decorator =
        other before self

      inline def after(other: Decorator): Decorator =
        self |<| other

      // =====| nested modifier fields |=====

      def modifyLabelModifier: NameFunctionModifier =
        NameFunctionModifier(GenLens[Decorator](_.labelModifier))

      def modifyFieldModifier: NameFunctionModifier =
        NameFunctionModifier(GenLens[Decorator](_.fieldModifier))

      def modifyWrappedModifier: NameFunctionModifier =
        NameFunctionModifier(GenLens[Decorator](_.wrappedModifier))

      def modifyDecorateLabel: IdFunctionK0CWidgetModifier =
        IdFunctionK0CWidgetModifier(GenLens[Decorator](_.decorateLabel))

      def modifyDecorateField: IdFunctionK2AVSubmitModifier =
        IdFunctionK2AVSubmitModifier(GenLens[Decorator](_.decorateField))

      def modifyDecorateWrapped: IdFunctionK2AVSubmitModifier =
        IdFunctionK2AVSubmitModifier(GenLens[Decorator](_.decorateWrapped))

    }
    object Decorator {
      val labelInFront: Decorator = Decorator()
      val labelAbove: Decorator = Decorator(labelModifier = NameFunction.const(display := "block"))
    }

    override protected def defaultDecorator: Decorator = Decorator.labelAbove

    override def generic[T: DecodeFromString, O](
        baseElement: => NodeElement,
    )(
        tToO: Option[T] => Valid[O],
    )(
        name: String,
        decorator: Decorator,
    ): AVWidget[Submit, String, O] = {
      val prettyName: String = name.split("-").map(_.capitalize).mkString(" ")
      val labelModifier: Modifier = decorator.labelModifier(name, prettyName)
      val fieldModifier: Modifier = decorator.fieldModifier(name, prettyName)
      val wrappedModifier: Modifier = decorator.wrappedModifier(name, prettyName)

      val baseLabel: CWidget = CWidget(label(s"$prettyName: ", labelModifier))
      val modifiedLabel: CWidget = decorator.decorateLabel(baseLabel)

      val baseField: AVSubmit[String, O] =
        AVWidget[Submit, String, O](
          (rh, s) =>
            baseElement(
              onKeyUp := { e =>
                val value = e.target.asInstanceOf[Dynamic].value.asInstanceOf[String]
                rh.setState(value)
              },
              value := s,
              fieldModifier,
            ),
          s => Option.when(s.nonEmpty)(s).traverse(DecodeFromString[T].decode(_)).flatMap(tToO),
        )
      val modifiedField: AVSubmit[String, O] = decorator.decorateField(baseField)

      val baseWrapped: AVSubmit[String, O] = (modifiedLabel >> modifiedField).wrapped(div(_, wrappedModifier))
      val modifiedWrapped: AVSubmit[String, O] = decorator.decorateWrapped(baseWrapped)

      modifiedWrapped
    }

  }

}
