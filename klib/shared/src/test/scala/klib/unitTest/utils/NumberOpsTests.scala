package klib.unitTest.utils

import zio.{test as _, *}
import zio.test.*
import zio.test.Assertion.*

import klib.unitTest.SpecUtils.*
import klib.utils.*

object NumberOpsTests extends DefaultKSpec {

  private val toStringCommasSpec: TestSpec = {
    val intSpec: TestSpec = {
      def makeTest(int: Int, exp: String): TestSpec =
        test(s"$int -> $exp")(assert(int.toStringCommas)(equalTo(exp)))

      suite("int")(
        makeTest(0, "0"),
        makeTest(1, "1"),
        makeTest(1234, "1,234"),
        makeTest(123456, "123,456"),
        makeTest(-123456, "-123,456"),
        makeTest(12345678, "12,345,678"),
      )
    }

    val longSpec: TestSpec = {
      def makeTest(long: Long, exp: String): TestSpec =
        test(s"$long -> $exp")(assert(long.toStringCommas)(equalTo(exp)))

      suite("long")(
        makeTest(0L, "0"),
        makeTest(1L, "1"),
        makeTest(1234L, "1,234"),
        makeTest(123456L, "123,456"),
        makeTest(-123456L, "-123,456"),
        makeTest(12345678L, "12,345,678"),
      )
    }

    val floatSpec: TestSpec = {
      def makeTest(float: Float, exp: String, expAlwaysShow: String): TestSpec =
        suite(BigDecimal(float).toString)(
          test(s"_.toStringCommas(false) -> $exp")(assert(float.toStringCommas(false))(equalTo(exp))),
          test(s"_.toStringCommas(true) -> $exp")(assert(float.toStringCommas(true))(equalTo(expAlwaysShow))),
        )

      suite("float")(
        makeTest(0f, "0", "0.0"),
        makeTest(1f, "1", "1.0"),
        makeTest(1234f, "1,234", "1,234.0"),
        makeTest(123456f, "123,456", "123,456.0"),
        makeTest(-123456f, "-123,456", "-123,456.0"),
        makeTest(12345678f, "12,345,678", "12,345,678.0"),
        makeTest(4.5f, "4.5", "4.5"),
        makeTest(1234.5f, "1,234.5", "1,234.5"),
      )
    }

    val doubleSpec: TestSpec = {
      def makeTest(double: Double, exp: String, expAlwaysShow: String): TestSpec =
        suite(BigDecimal(double).toString)(
          test(s"_.toStringCommas(false) -> $exp")(assert(double.toStringCommas(false))(equalTo(exp))),
          test(s"_.toStringCommas(true) -> $exp")(assert(double.toStringCommas(true))(equalTo(expAlwaysShow))),
        )

      suite("double")(
        makeTest(0d, "0", "0.0"),
        makeTest(1d, "1", "1.0"),
        makeTest(1234d, "1,234", "1,234.0"),
        makeTest(123456d, "123,456", "123,456.0"),
        makeTest(-123456d, "-123,456", "-123,456.0"),
        makeTest(12345678d, "12,345,678", "12,345,678.0"),
        makeTest(1.2d, "1.2", "1.2"),
        makeTest(1234.5d, "1,234.5", "1,234.5"),
      )
    }

    suite("toStringCommas")(
      intSpec,
      longSpec,
      floatSpec,
      doubleSpec,
    )
  }

  override def spec: TestSpec =
    suite("NumberOpsTests")(
      toStringCommasSpec,
    )

}
