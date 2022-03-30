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
      counter2: Int,
      text: String,
  )

  val testPage: Page =
    Page
      .builder("test")
      .constEnv { Env("test", 0, 0, "init") }
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

        val counter: VWidget[Int, Int] =
          CWidget("Counter: ") >>
            incButton("-", i => (i - 1).max(0)) >>
            Widget[Int] { s => span(display := "inline-block", textAlign := "center", width := "25px")(s.toString) }.valueFromState(identity) >>
            incButton("+", _ + 1)

        counter
          .mapValue(_ * 2)
          .debugStateAndValue
          .zoomOut[Env](_.counter)
      }
      .logA

  override val routeMatcher: RouteMatcher[Page] =
    RouteMatcher.oneOf(
      RouteMatcher.consumeEntirePath.as(testPage),
    )

}
