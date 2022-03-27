package klib.toy

import monocle.macros.GenLens

import klib.utils.*
import klib.web.*
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
        Widget[String](s => div(s)).imapState(GenLens[Env](_.str)) >>
          Widget[String](s => div(s + " : " + s)).imapState(GenLens[Env](_.str)) >> {
            CWidget(span("[")) >>
              Widget[String](s => span(s.toUpperCase)).imapState(GenLens[Env](_.str)) >>
              CWidget(span("]"))
          }.wrapped(div(padding := "25px")(_)) >>
          CWidget("counter: ") >>
          Widget[Int](i => span(color.red)(i.toString)).imapState(GenLens[Env](_.counter)) >>
          AWidget[Nothing, Int] { (rh, s) =>
            button(
              onClick := { _ => rh.modifyState(_ + 1) },
              "+",
            )
          }.imapState(GenLens[Env](_.counter)) >>
          AWidget[Nothing, Int] { (rh, s) =>
            button(
              onClick := { _ => rh.modifyState(_ - 1) },
              "-",
            )
          }.imapState(GenLens[Env](_.counter))
      }
      .logA

  override val routeMatcher: RouteMatcher[Page] =
    RouteMatcher.oneOf(
      RouteMatcher.consumeEntirePath.as(testPage),
    )

}
