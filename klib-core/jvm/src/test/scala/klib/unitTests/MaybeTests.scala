package klib.unitTests

import org.scalatest.funspec.AnyFunSpec

import klib.Implicits._
import klib.fp.types._

final class MaybeTests extends AnyFunSpec {

  describe("ordering") {

    val tests: List[(List[Maybe[Int]], List[Maybe[Int]])] =
      List(
        (
          List(
            1.some,
            2.some,
            3.some,
          ),
          List(
            1.some,
            2.some,
            3.some,
          ),
        ),
        (
          List(
            3.some,
            2.some,
            1.some,
          ),
          List(
            1.some,
            2.some,
            3.some,
          ),
        ),
        (
          List(
            3.some,
            2.some,
            1.some,
          ),
          List(
            1.some,
            2.some,
            3.some,
          ),
        ),
        (
          List(
            3.some,
            2.some,
            None,
            1.some,
          ),
          List(
            None,
            1.some,
            2.some,
            3.some,
          ),
        ),
        (
          List(
            3.some,
            None,
            2.some,
            1.some,
            None,
          ),
          List(
            None,
            None,
            1.some,
            2.some,
            3.some,
          ),
        ),
      )

    tests.zipWithIndex.foreach {
      case ((init, exp), idx) =>
        it((idx + 1).toString) {
          assertResult(exp)(init.sorted)
        }
    }

  }

}
