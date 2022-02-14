package klib.unitTest.utils

import cats.data.NonEmptyList
import zio.{test as _, *}
import zio.test.*
import zio.test.Assertion.*

import klib.utils.*
import klib.unitTest.SpecUtils.*

object ZIOOpsTests extends DefaultKSpec {

  private val traverseTests: TestSpec = {
    val singleSpec: TestSpec = {
      def makeTest(name: String)(zios: IO[Int, String]*)(assertion: Assertion[Either[NonEmptyList[Int], List[String]]]): TestSpec =
        test(name)(
          for {
            res <- ZIO.traverse(zios.toList)(identity).either
          } yield assert(res)(assertion),
        )

      suite("single")(
        makeTest("empty")()(isRight(equalTo(Nil))),
        makeTest("1 success")(
          ZIO.succeed("A"),
        )(isRight(equalTo(List("A")))),
        makeTest("3 successes")(
          ZIO.succeed("A"),
          ZIO.succeed("B"),
          ZIO.succeed("C"),
        )(isRight(equalTo(List("A", "B", "C")))),
        makeTest("1 failure")(
          ZIO.fail(1),
        )(isLeft(equalTo(NonEmptyList.of(1)))),
        makeTest("3 failures")(
          ZIO.fail(1),
          ZIO.fail(2),
          ZIO.fail(3),
        )(isLeft(equalTo(NonEmptyList.of(1, 2, 3)))),
        makeTest("3 successes + 3 failures")(
          ZIO.succeed("A"),
          ZIO.fail(1),
          ZIO.succeed("B"),
          ZIO.fail(2),
          ZIO.succeed("C"),
          ZIO.fail(3),
        )(isLeft(equalTo(NonEmptyList.of(1, 2, 3)))),
      )
    }
    val nelSpec: TestSpec = {
      def makeTest(
          name: String,
      )(zios: IO[NonEmptyList[Int], String]*)(assertion: Assertion[Either[NonEmptyList[Int], List[String]]]): TestSpec =
        test(name)(
          for {
            res <- ZIO.traverseNEL(zios.toList)(identity).either
          } yield assert(res)(assertion),
        )

      suite("nel")(
        makeTest("empty")()(isRight(equalTo(Nil))),
        makeTest("1 success")(
          ZIO.succeed("A"),
        )(isRight(equalTo(List("A")))),
        makeTest("3 successes")(
          ZIO.succeed("A"),
          ZIO.succeed("B"),
          ZIO.succeed("C"),
        )(isRight(equalTo(List("A", "B", "C")))),
        makeTest("1 failure")(
          ZIO.fail(1 :: 2 :: NENil),
        )(isLeft(equalTo(NonEmptyList.of(1, 2)))),
        makeTest("3 failures")(
          ZIO.fail(1 :: 2 :: NENil),
          ZIO.fail(3 :: 4 :: NENil),
          ZIO.fail(5 :: 6 :: NENil),
        )(isLeft(equalTo(NonEmptyList.of(1, 2, 3, 4, 5, 6)))),
        makeTest("3 successes + 3 failures")(
          ZIO.succeed("A"),
          ZIO.fail(1 :: 2 :: NENil),
          ZIO.succeed("B"),
          ZIO.fail(3 :: 4 :: NENil),
          ZIO.succeed("C"),
          ZIO.fail(5 :: 6 :: NENil),
        )(isLeft(equalTo(NonEmptyList.of(1, 2, 3, 4, 5, 6)))),
      )
    }

    suite("traverse")(
      singleSpec,
      nelSpec,
    )
  }

  override def spec: TestSpec =
    suite("ZIOOpsTests")(
      traverseTests,
    )

}
