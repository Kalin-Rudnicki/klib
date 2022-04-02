package klib.web.widgets

import klib.utils.*
import klib.web.*
import klib.web.VDom.{given, *}
import klib.web.VDomBuilders.{given, *}
import klib.web.widgets.CommonRaises.*

object FormWidgets {

  final case class SubmitButtonDecorator(
      buttonClassModifiers: List[String] = Nil,
      buttonModifiers: BeforeAfterModifier = BeforeAfterModifier(),
  )

  def submitButton(
      text: String = "Submit",
      decorator: SubmitButtonDecorator = SubmitButtonDecorator(),
  ): CAWidget[Submit] =
    PWidget.withAction[Submit].withNoState.withNoValue.withElementR { rh =>
      button(
        decorator.buttonModifiers.before,
      )(
        text,
        onClick := { _ => rh.raiseAction(SubmitOr.Submit) },
      )(
        decorator.buttonModifiers.after,
        ClassName.b("widget-submit-button", decorator.buttonClassModifiers),
      )
    }

}
