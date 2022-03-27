package klib.toy

import cats.data.NonEmptyList
import cats.syntax.either.*

import klib.utils.*
import klib.web.{given, *}
import klib.web.VDom.{given, *}
import klib.web.VDomBuilders.*

object Main extends PageApp {

  final case class Env(
      str: String,
      counter: Int,
      counter2: Int,
  )

  val testPage: Page =
    Page
      .builder("test")
      .constEnv { Env("test", 0, 0) }
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

        val counter: Widget[Int] =
          CWidget("Counter: ") >>
            incButton("-", i => (i - 1).max(0)) >>
            Widget[Int] { s => span(display := "inline-block", textAlign := "center", width := "25px")(s.toString) } >>
            incButton("+", _ + 1) >>
            CWidget(div("Here is some more text..."))

        val all: VWidget[Env, Int] = {
          header >>
            counter.zoomOut[Env](_.counter)
        }.eitherValueFromState(env => Option.when(env.counter >= 0)(env.counter).toRight(NonEmptyList.one("Counter must be >= 0")))

        all.placeAfterWithEitherValue { e =>
          CWidget(div(s"Value: $e"))
        } >>
          counter.zoomOut[Env](_.counter2)
      }
      .logA

  override val routeMatcher: RouteMatcher[Page] =
    RouteMatcher.oneOf(
      RouteMatcher.consumeEntirePath.as(testPage),
    )

}
