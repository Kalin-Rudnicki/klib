package klib.web.widgets

import cats.syntax.traverse.*
import org.scalajs.dom.console
import scala.scalajs.js.Dynamic

import klib.fp.typeclass.DecodeFromString
import klib.utils.*
import klib.web.*
import klib.web.BasicPredef.{given, *}

object TextWidgets {

  private def genericTextW[T: DecodeFromString](
      tagName: String,
      modifiers: List[Modifier],
  ): AVWidget[SubmitOr.Submit.type, String, Option[T]] =
    AVWidget[SubmitOr.Submit.type, String, Option[T]](
      (rh, s) =>
        NodeElement(
          tagName,
          modifiers ::
            List[Modifier](
              onKeyUp := { e =>
                val value = e.target.asInstanceOf[Dynamic].value.asInstanceOf[String]
                rh.setState(value)
              },
              value := s,
            ) ::
            Nil,
        ),
      s => Option.when(s.nonEmpty)(s).traverse(DecodeFromString[T].decode),
    )

  def inputW[T: DecodeFromString](
      modifiers: List[Modifier] = Nil,
  ): AVWidget[SubmitOr.Submit.type, String, Option[T]] =
    genericTextW("input", modifiers)

  def textareaW[T: DecodeFromString](
      modifiers: List[Modifier] = Nil,
  ): AVWidget[SubmitOr.Submit.type, String, Option[T]] =
    genericTextW("textarea", modifiers)

}
