package klib.web.widgets

import cats.syntax.either.*
import cats.syntax.option.*

import klib.utils.*
import klib.web.*
import klib.web.VDom.{given, *}
import klib.web.VDomBuilders.{given, *}
import klib.web.widgets.CommonRaises.*

object EnumWidgets {

  final case class RadioGroupDecorator(
      groupClassModifiers: List[String] = Nil,
      buttonClassModifiers: List[String] = Nil,
      groupModifier: BeforeAfterModifier = BeforeAfterModifier(),
      buttonModifier: BeforeAfterModifier = BeforeAfterModifier(),
      buttonModifierFirst: BeforeAfterModifier = BeforeAfterModifier(),
      buttonModifierLast: BeforeAfterModifier = BeforeAfterModifier(),
      buttonModifierSelected: BeforeAfterModifier = BeforeAfterModifier(),
  )

  def radioGroup[E](
      values: Seq[E],
      decorator: RadioGroupDecorator = RadioGroupDecorator(),
      eToString: E => String = (e: E) => e.toString,
  ): VWidget[Option[E], Option[E]] =
    AVWidget[Nothing, Option[E], Option[E]](
      { (rh, s) =>
        val numValues = values.size

        span(
          decorator.groupModifier.before,
        )(
          values.zipWithIndex.map { (e, i) =>
            val isFirst = i == 0
            val isLast = i == numValues - 1
            val isSelected = s.contains(e)

            span(
              decorator.buttonModifier.before,
              Option.when(isFirst)(decorator.buttonModifierFirst.before),
              Option.when(isLast)(decorator.buttonModifierLast.before),
              Option.when(isSelected)(decorator.buttonModifierSelected.before),
            )(
              eToString(e),
              onClick := { _ => rh.setState(e.some) },
              onContextMenu := { e =>
                e.preventDefault()
                if (isSelected) rh.setState(None)
              },
              ClassName.be(
                "widget-radio-group",
                "element",
                Option.when(isFirst)("first"),
                Option.when(isLast)("last"),
                Option.when(isSelected)("selected"),
                decorator.buttonClassModifiers,
              ),
            )(
              decorator.buttonModifier.after,
              Option.when(isFirst)(decorator.buttonModifierFirst.after),
              Option.when(isLast)(decorator.buttonModifierLast.after),
              Option.when(isSelected)(decorator.buttonModifierSelected.after),
            )
          },
          ClassName.b(
            "widget-radio-group",
            (if (s.isEmpty) "empty" else "non-empty").some,
            decorator.groupClassModifiers,
          ),
        )(
          decorator.groupModifier.after,
        )
      },
      _.asRight,
    )

}
