package klib.web.widgets

import cats.Monoid
import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import scala.scalajs.js.Dynamic

import klib.fp.typeclass.DecodeFromString
import klib.utils.*
import klib.web.*
import klib.web.VDom.{given, *}
import klib.web.VDomBuilders.{given, *}
import klib.web.widgets.CommonRaises.*

object TextWidgets {

  final case class TextFieldDecorator(
      textFieldClassModifiers: List[String] = Nil,
      textFeidldModifiers: BeforeAfterModifier = BeforeAfterModifier(),
  ) extends FocusableDecorator[TextFieldDecorator] { self =>
    override def getSelf: TextFieldDecorator = self
  }

  sealed abstract class TextFieldBuilder1(tagName: String, requireCtrlForSubmit: Boolean) {

    def apply[T: DecodeFromString](decorator: TextFieldDecorator = TextFieldDecorator()): AVWidget[Submit, String, Option[T]] =
      AVWidget[Submit, String, Option[T]](
        (rh, s) =>
          NodeElement(tagName)(
            decorator.textFeidldModifiers.before,
          )(
            onKeyUp := { e =>
              val value = e.target.asInstanceOf[Dynamic].value.asInstanceOf[String]
              val raises: List[Raise[Submit, String]] =
                Raise.setState(value) ::
                  Option.when(e.key == "Enter" && (!requireCtrlForSubmit || e.ctrlKey))(Raise.Action(SubmitOr.Submit)).toList

              rh.raiseMany(raises)
            },
            value := s,
            ClassName.b("widget-text", tagName.some, decorator.textFieldClassModifiers),
          )(
            decorator.textFeidldModifiers.after,
          ),
        s =>
          Option
            .when(s.nonEmpty)(s)
            .traverse {
              DecodeFromString[T].decode(_)
            },
      )

  }

  object input extends TextFieldBuilder1("input", false)
  object textArea extends TextFieldBuilder1("textarea", true)

}
