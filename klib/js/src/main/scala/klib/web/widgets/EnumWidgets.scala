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
      buttonModifier: BeforeAfterModifier = BeforeAfterModifier(),
      buttonModifierFirst: BeforeAfterModifier = BeforeAfterModifier(),
      buttonModifierLast: BeforeAfterModifier = BeforeAfterModifier(),
      buttonModifierSelected: BeforeAfterModifier = BeforeAfterModifier(),
      groupModifier: BeforeAfterModifier = BeforeAfterModifier(),
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
            )(
              decorator.buttonModifier.after,
              Option.when(isFirst)(decorator.buttonModifierFirst.after),
              Option.when(isLast)(decorator.buttonModifierLast.after),
              Option.when(isSelected)(decorator.buttonModifierSelected.after),
            )
          },
        )(
          decorator.groupModifier.after,
        )
      },
      _.asRight,
    )

}
