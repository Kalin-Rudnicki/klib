package klib.unitTest.utils

import cats.syntax.option.*
import zio.test.Assertion.*
import zio.test.*

import klib.unitTest.SpecUtils.*
import klib.utils.*
import klib.utils.ColorString.Implicits.*
import klib.utils.commandLine.parse.*

object ColorStringTests extends DefaultKSpec {

  private object TestData {
    val Red: ColorString = "Red".toColorString.red
    val Blue: ColorString = "Blue".toColorString.blue
    val Green: ColorString = "Green".toColorString.green
  }

  import TestData.*

  private def makeTest(
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
      makeTest(
        "ColorString.Simple",
        Red,
        "[[Red]]Red[[Default]]",
      ),
      makeTest(
        "ColorString.Simple + ColorString.Simple",
        Red + Blue,
        "[[Red]]Red[[Blue]]Blue[[Default]]",
      ),
    )

  private val interpolationSpec: TestSpec =
    suite("interpolation")(
      makeTest(
        "touching",
        color"$Red$Blue",
        "[[Red]]Red[[Blue]]Blue[[Default]]",
      ),
      makeTest(
        "not touching",
        color"$Red-$Blue-$Green",
        "[[Red]]Red[[Default]]-[[Blue]]Blue[[Default]]-[[Green]]Green[[Default]]",
      ),
      makeTest(
        "spaces on outside",
        color"--$Red-$Blue--",
        "--[[Red]]Red[[Default]]-[[Blue]]Blue[[Default]]--",
      ),
      makeTest(
        "modified after interpolation",
        color"$Red-$Blue-$Green".yellow,
        "[[Red]]Red[[Yellow]]-[[Blue]]Blue[[Yellow]]-[[Green]]Green[[Default]]",
      ),
      makeTest(
        "nested with same color",
        color"--$Red--".red,
        s"[[Red]]--Red--[[Default]]",
      ),
      makeTest(
        "nested 1",
        color"--${color"--$Red-$Blue--"}--",
        "----[[Red]]Red[[Default]]-[[Blue]]Blue[[Default]]----",
      ),
      makeTest(
        "nested 2",
        color"--${color"--$Red-$Blue--".green}--",
        "--[[Green]]--[[Red]]Red[[Green]]-[[Blue]]Blue[[Green]]--[[Default]]--",
      ),
      makeTest(
        "nested 3",
        color"--${color"--$Red-$Blue--"}--".yellow,
        "[[Yellow]]----[[Red]]Red[[Yellow]]-[[Blue]]Blue[[Yellow]]----[[Default]]",
      ),
      makeTest(
        "nested 4",
        color"--${color"--$Red-$Blue--".green}--".yellow,
        "[[Yellow]]--[[Green]]--[[Red]]Red[[Green]]-[[Blue]]Blue[[Green]]--[[Yellow]]--[[Default]]",
      ),
    )

  override def spec: TestSpec =
    suite("ColorStringTests")(
      miscSpec,
      interpolationSpec,
    ) @@ TestAspect.sequential // REMOVE

}
