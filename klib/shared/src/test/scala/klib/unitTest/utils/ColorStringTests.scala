package klib.unitTest.utils

import cats.syntax.option.*
import zio.test.Assertion.*
import zio.test.*

import klib.unitTest.SpecUtils.*
import klib.utils.{*, given}
import klib.utils.commandLine.parse.*

object ColorStringTests extends DefaultKSpec {

  private object TestData {
    val Red: ColorString = "Red".red
    val Blue: ColorString = "Blue".blue
    val Green: ColorString = "Green".green
  }

  import TestData.*

  private def makeToStringTest(
      name: String,
      colorString: => ColorString,
      exp: String,
  ): TestSpec =
    test(name) {
      val cs = colorString
      assert(cs.toString(ColorMode.Show))(equalTo(exp))
    }

  private val miscSpec: TestSpec =
    suite("misc")(
      makeToStringTest(
        "no color",
        "NoColor".dflt,
        "NoColor",
      ),
      makeToStringTest(
        "ColorString.Simple",
        Red,
        "[[Red]]Red[[Default]]",
      ),
      makeToStringTest(
        "ColorString.Simple + ColorString.Simple",
        Red + Blue,
        "[[Red]]Red[[Blue]]Blue[[Default]]",
      ),
    )

  private val interpolationSpec: TestSpec =
    suite("interpolation")(
      makeToStringTest(
        "touching",
        color"$Red$Blue",
        "[[Red]]Red[[Blue]]Blue[[Default]]",
      ),
      makeToStringTest(
        "not touching",
        color"$Red-$Blue-$Green",
        "[[Red]]Red[[Default]]-[[Blue]]Blue[[Default]]-[[Green]]Green[[Default]]",
      ),
      makeToStringTest(
        "spaces on outside",
        color"--$Red-$Blue--",
        "--[[Red]]Red[[Default]]-[[Blue]]Blue[[Default]]--",
      ),
      makeToStringTest(
        "modified after interpolation",
        color"$Red-$Blue-$Green".yellow,
        "[[Red]]Red[[Yellow]]-[[Blue]]Blue[[Yellow]]-[[Green]]Green[[Default]]",
      ),
      makeToStringTest(
        "nested with same color",
        color"--$Red--".red,
        s"[[Red]]--Red--[[Default]]",
      ),
      makeToStringTest(
        "nested 1",
        color"--${color"--$Red-$Blue--"}--",
        "----[[Red]]Red[[Default]]-[[Blue]]Blue[[Default]]----",
      ),
      makeToStringTest(
        "nested 2",
        color"--${color"--$Red-$Blue--".green}--",
        "--[[Green]]--[[Red]]Red[[Green]]-[[Blue]]Blue[[Green]]--[[Default]]--",
      ),
      makeToStringTest(
        "nested 3",
        color"--${color"--$Red-$Blue--"}--".yellow,
        "[[Yellow]]----[[Red]]Red[[Yellow]]-[[Blue]]Blue[[Yellow]]----[[Default]]",
      ),
      makeToStringTest(
        "nested 4",
        color"--${color"--$Red-$Blue--".green}--".yellow,
        "[[Yellow]]--[[Green]]--[[Red]]Red[[Green]]-[[Blue]]Blue[[Green]]--[[Yellow]]--[[Default]]",
      ),
    )

  private val splitSpec: TestSpec =
    suite("split")(
      makeToStringTest(
        "simple",
        "1,2,3".toColorString.red.split(",").csMkString("(", ",", ")"),
        "([[Red]]1[[Default]],[[Red]]2[[Default]],[[Red]]3[[Default]])",
      ),
    )

  override def spec: TestSpec =
    suite("ColorStringTests")(
      miscSpec,
      interpolationSpec,
      splitSpec,
    )

}
