package klib.toy

import cats.data.NonEmptyList
import cats.syntax.either.*

import klib.utils.*
import klib.web.{given, *}
import klib.web.BasicPredef.{given, *}
import klib.web.widgets.*

object Main extends PageApp {

  final case class Env(
      str: String,
      counter: Int,
  )

  val testPage: Page =
    Page
      .builder("test")
      .constEnv { Env("", 0) }
      .titleF(env => s"${env.str} : ${env.counter}")
      .body {
        def incButton(text: String, modify: Int => Int): Widget[Int] =
          AWidget[Nothing, Int] { (rh, s) =>
            button(
              text,
              backgroundColor.aqua,
              padding := "3px 10px",
              borderRadius := "10px",
              ClassName.be("my-button", "stuff", Option.when(s > 5)("more-than-5")),
              onClick := { _ => rh.modifyState(modify) },
            )
          }

        val header: Widget[Env] =
          CWidget(h1("HEADER")) >>
            Widget[Env](s => div(s"${s.str} : ${s.counter}"))

        val counter: VWidget[Int, Int] = {
          incButton("-", i => (i - 1).max(0)) >>
            Widget[Int] { s => span(display := "inline-block", textAlign := "center", width := "25px")(s.toString) }.setValueS(identity) >>
            incButton("+", _ + 1)
        }.labeledAbove("counter")

        {
          header >>
            counter
              .mapValueV(_ * 2)
              .debugStateAndValue
              .zoomOut[Env](_.counter) >>
            TextWidgets
              .input[String]()
              .labeledAbove("field-1")
              .zoomOut[Env](_.str) >>
            TextWidgets
              .input[Int](TextWidgets.TextFieldDecorator.after(`type`.number))
              .required
              .labeledAbove("field-2")
              .zoomOut[Env](_.str) >>
            TextWidgets
              .textArea[String]()
              .required
              .labeledAbove("field-3")
              .zoomOut[Env](_.str)
        }.debugStateAndValue
      }
      .logA

  override val routeMatcher: RouteMatcher[Page] =
    RouteMatcher.oneOf(
      RouteMatcher.consumeEntirePath.as(testPage),
    )

}
