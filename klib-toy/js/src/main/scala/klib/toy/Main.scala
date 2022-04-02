package klib.toy

import cats.data.NonEmptyList
import cats.syntax.either.*
import io.circe.Encoder
import io.circe.generic.auto.*

import klib.utils.*
import klib.web.{given, *}
import klib.web.BasicPredef.{given, *}
import klib.web.widgets.*

object Main extends PageApp {

  final case class Env(
      exCounter: Int,
      exInputString: String,
      exInputInt: String,
      exInputStringDoubleReference: String,
      exFood: Option[Food],
  )

  implicit class PWidgetOps[A, SG, SS <: SG, V](widget: PWidget[A, SG, SS, V]) {

    def inSection: PWidget[A, SG, SS, V] =
      widget.wrapped(div(margin := "10px", padding := "10px", border := "1px solid black", _))

  }

  enum Food { case Pizza, Burger, Pasta, Burrito }
  object Food {
    implicit val encoder: Encoder[Food] =
      Encoder[String].contramap[Food](_.toString)
  }

  val testPage: Page =
    Page
      .builder("test")
      .constEnv {
        Env(
          exCounter = 0,
          exInputString = "exInputString",
          exInputInt = "0",
          exInputStringDoubleReference = "exInputStringDoubleReference",
          exFood = None,
        )
      }
      .constTitle("Examples")
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

        val header: CWidget =
          CWidget(h1("Examples:"))

        val exCounter: VWidget[Env, Int] = {
          incButton("-", i => (i - 1).max(0)) >>
            Widget[Int] { s => span(display := "inline-block", textAlign := "center", width := "25px")(s.toString) }.setValueS(identity) >>
            incButton("+", _ + 1)
        }
          .labeledInFront("ex-counter")
          .zoomOut[Env](_.exCounter)
          .inSection

        val exInputString: AVWidget[Submit, Env, String] =
          TextWidgets
            .input[String]()
            .required
            .labeledAbove("ex-input-string")
            .zoomOut[Env](_.exInputString)
            .inSection

        val exInputInt: AVWidget[Submit, Env, Option[Int]] =
          TextWidgets
            .input[Int]()
            .labeledAbove("ex-input-int")
            .zoomOut[Env](_.exInputInt)
            .inSection

        val exInputStringDoubleReference: AVWidget[Submit, Env, (String, Option[String])] = {
          TextWidgets
            .input[String]()
            .required
            .mapValueV(v => s"1: $v")
            .labeledAbove("ex-input-string-double-reference-1") >>
            TextWidgets
              .input[String]()
              .mapValueV(_.map(v => s"2: $v"))
              .labeledAbove("ex-input-string-double-reference-2")
        }
          .zoomOut[Env](_.exInputStringDoubleReference)
          .inSection

        val exFood: VWidget[Env, Option[Food]] =
          EnumWidgets
            .radioGroup(
              Food.values,
              EnumWidgets.RadioGroupDecorator(
                buttonModifier = BeforeAfterModifier.after(padding := "5px", border := "2px solid black", cursor.pointer, userSelect.none),
                buttonModifierSelected = BeforeAfterModifier.after(backgroundColor.aqua),
              ),
            )
            .labeledAbove("ex-food", _.focusLabelModifierAfter.after(marginBottom := "5px"))
            .zoomOut[Env](_.exFood)
            .inSection

        {
          header >>
            exCounter >>
            exInputString >>
            exInputInt >>
            exInputStringDoubleReference >>
            exFood
        }.debugStateAndValueJson
      }
      .logA

  override val routeMatcher: RouteMatcher[Page] =
    RouteMatcher.oneOf(
      RouteMatcher.consumeEntirePath.as(testPage),
    )

}
