package klib.toy

import cats.data.NonEmptyList
import cats.syntax.either.*

import klib.utils.*
import klib.web.{given, *}
import klib.web.VDom.given
import klib.web.VDomBuilders.*

object Main extends PageApp {

  final case class Env(
      str: String,
      counter: Int,
  )

  val testPage: Page =
    Page
      .builder("test")
      .constEnv {
        Env(
          "test",
          0,
        )
      }
      .titleF(env => s"${env.str} : ${env.counter}")
      .body {
        def incButton(text: String, modify: Int => Int): Widget[Int] =
          AWidget[Nothing, Int] { (rh, s) =>
            button(
              text,
              onClick := { _ => rh.modifyState(modify) },
            )
          }

        val header: Widget[Env] =
          CWidget(div("HEADER")) >>
            Widget[Env](s => div(s"${s.str} : ${s.counter}"))

        val counter: Widget[Int] =
          CWidget("Counter: ") >>
            incButton("+", _ + 1) >>
            Widget[Int] { s => span(display := "inline-block", textAlign := "center", width := "25px")(s.toString) } >>
            incButton("-", _ - 1)

        val all: VWidget[Env, Int] = {
          header >>
            counter.zoomOut[Env](_.counter)
        }.eitherValueFromState(env => Option.when(env.counter >= 0)(env.counter).toRight(NonEmptyList.one("Counter must be >= 0")))

        all.placeBeforeWithEitherValue { e =>
          CWidget(div(s"Value: $e"))
        }
      }
      .logA

  override val routeMatcher: RouteMatcher[Page] =
    RouteMatcher.oneOf(
      RouteMatcher.consumeEntirePath.as(testPage),
    )

}
