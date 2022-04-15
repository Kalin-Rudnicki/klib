package klib.unitTest.fp.typeclass

import cats.syntax.option.*
import java.time.*
import zio.test.*
import zio.test.Assertion.*

import klib.fp.typeclass.DecodeFromString
import klib.unitTest.SpecUtils.*
import klib.utils.*
import klib.utils.InfiniteSet.*

object DecodeFromStringTests extends DefaultKSpec {

  private val temporalSpec: TestSpec = {
    val localDateSpec: TestSpec = {
      def makeSuite(currentYear: Int, futureTolerance: Int, name: Option[String] = None)(
          tests: (String, String, Option[LocalDate])*,
      ): TestSpec =
        suite(name.getOrElse(s"currentYear: $currentYear, futureTolerance: $futureTolerance"))(
          tests.map { (testName, input, expected) =>
            test(testName) {
              val res = DecodeFromString.configurableLocalDateDecodeString(currentYear, futureTolerance).decode(input)
              val assertion =
                expected match {
                  case Some(expected) => isRight(equalTo(expected))
                  case None           => isLeft
                }

              assert(res)(assertion)
            }
          } *,
        )

      def makeGuessSuite(currentYear: Int, futureTolerance: Int)(
          `start-of-century`: Int,
          `past-start-of-century`: Int,
          `current-year`: Int,
          `current-year - 1`: Int,
          `current-year + 1`: Int,
          `future-year`: Int,
          `future-year - 1`: Int,
          `future-year + 1`: Int,
          `before-end-of-century`: Int,
          `end-of-century`: Int,
      ): TestSpec = {
        def makeTest(name: String, _inputYear: Int, expYear: Int): (String, String, Option[LocalDate]) = {
          val inputYear = (_inputYear % 100).toString.alignRight(2, '0')
          (s"$name ($inputYear -> $expYear)", s"1/1/$inputYear", LocalDate.of(expYear, 1, 1).some)
        }

        makeSuite(currentYear, futureTolerance)(
          makeTest("start-of-century", 0, `start-of-century`),
          makeTest("past-start-of-century", 10, `past-start-of-century`),
          makeTest("current-year", currentYear, `current-year`),
          makeTest("current-year - 1", currentYear - 1, `current-year - 1`),
          makeTest("current-year + 1", currentYear + 1, `current-year + 1`),
          makeTest("future-year", currentYear + futureTolerance, `future-year`),
          makeTest("future-year - 1", currentYear + futureTolerance - 1, `future-year - 1`),
          makeTest("future-year + 1", currentYear + futureTolerance + 1, `future-year + 1`),
          makeTest("before-end-of-century", 90, `before-end-of-century`),
          makeTest("end-of-century", 99, `end-of-century`),
        )
      }

      suite("localDate")(
        makeSuite(0, 0, "general".some)(
          ("empty-string", "", None),
          ("format-us", "01/02/0003", LocalDate.of(3, 1, 2).some),
          ("format-other", "01-02-0003", LocalDate.of(3, 2, 1).some),
          ("format-year-first", "0003-02-01", LocalDate.of(3, 2, 1).some),
          ("invalid-month", "13/02/0003", None),
          ("invalid-day", "01/32/0003", None),
        ),
        makeGuessSuite(2020, 10)(
          `start-of-century` = 2000,
          `past-start-of-century` = 2010,
          `current-year` = 2020,
          `current-year - 1` = 2019,
          `current-year + 1` = 2021,
          `future-year` = 2030,
          `future-year - 1` = 2029,
          `future-year + 1` = 1931,
          `before-end-of-century` = 1990,
          `end-of-century` = 1999,
        ),
        makeGuessSuite(2095, 10)(
          `start-of-century` = 2100,
          `past-start-of-century` = 2010,
          `current-year` = 2095,
          `current-year - 1` = 2094,
          `current-year + 1` = 2096,
          `future-year` = 2105,
          `future-year - 1` = 2104,
          `future-year + 1` = 2006,
          `before-end-of-century` = 2090,
          `end-of-century` = 2099,
        ),
        makeGuessSuite(2020, 0)(
          `start-of-century` = 2000,
          `past-start-of-century` = 2010,
          `current-year` = 2020,
          `current-year - 1` = 2019,
          `current-year + 1` = 1921,
          `future-year` = 2020,
          `future-year - 1` = 2019,
          `future-year + 1` = 1921,
          `before-end-of-century` = 1990,
          `end-of-century` = 1999,
        ),
      )
    }

    suite("temporal")(
      localDateSpec,
    )
  }

  override def spec: TestSpec =
    suite("DecodeFromStringTests")(
      temporalSpec,
    )

}
