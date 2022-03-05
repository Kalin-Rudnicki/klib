package klib.unitTest.utils

import cats.data.NonEmptyList
import scala.reflect.ClassTag
import zio.{test as _, *}
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import klib.unitTest.SpecUtils.*
import klib.utils.*

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

  private def refArrayStream[A: ClassTag](as: A*): UStream[A] =
    for {
      ref <- ZStream.fromZIO(Ref.make(0))
      array = as.toArray
      elem <- ZStream.repeatZIOOption {
        for {
          idx <- ref.get
          _ <- ZIO.cond(idx < array.length, (), None)
          elem = array(idx)
          _ <- ref.set(idx + 1)
        } yield elem
      }
    } yield elem

  private val refArrayStreamSpec: TestSpec = {
    val worksSpec: TestSpec = {
      def makeTest[A: ClassTag](name: String)(as: A*): TestSpec =
        test(name) {
          refArrayStream(as*)
            .runFold(List.empty[A]) { (l, a) => a :: l }
            .map { elems => assert(elems.reverse)(equalTo(as.toList)) }
        }

      suite("works")(
        makeTest[Int]("empty")(),
        makeTest("single")(1),
        makeTest("multi")(1, 2, 3, 4, 5),
      )
    }

    suite("refArrayStream")(
      worksSpec,
    )
  }

  private val zStreamIteratorSpec: TestSpec = {
    val takeSpec: TestSpec = {
      def makeTest[A: ClassTag](name: String)(as: A*)(gets: (Int, List[A])*): TestSpec =
        test(name) {
          refArrayStream(as*).toZStreamIterator.flatMap { iter =>
            def rec(idx: Int, gets: List[(Int, List[A])]): UIO[TestResult] =
              gets match {
                case (num, exp) :: tail =>
                  for {
                    nel <- iter.take(num)
                    list = nel.fold(List.empty[A])(_.toList)
                    myResult = assert(list)(equalTo(exp).imap(s"gets[$idx]", identity))
                    tailResult <- rec(idx + 1, tail)
                  } yield myResult && tailResult
                case Nil =>
                  ZIO.succeed(assertCompletes)
              }

            rec(0, gets.toList)
          }
        }

      suite("take")(
        makeTest[Int]("empty")()(
          1 -> Nil,
          2 -> Nil,
          3 -> Nil,
        ),
        makeTest("single")(1)(
          1 -> List(1),
          2 -> Nil,
          3 -> Nil,
        ),
        makeTest("multi")(1, 2, 3, 4, 5)(
          1 -> List(1),
          2 -> List(2, 3),
          3 -> List(4, 5),
        ),
      )
    }

    val takeForeachSpec: TestSpec = {
      val buildListSpec: TestSpec = {
        def makeTest[A: ClassTag](name: String)(as: A*)(nums: Int*)(exp: A*): TestSpec = {
          def rec(ref: Ref[List[A]], iter: ZStreamIterator[Any, Nothing, A], nums: List[Int]): UIO[Unit] =
            nums match {
              case head :: tail =>
                iter
                  .takeForeach(head) { a => ref.update(a :: _) }
                  .flatMap {
                    case true  => rec(ref, iter, tail)
                    case false => ZIO.unit
                  }
              case Nil =>
                ZIO.unit
            }

          test(name) {
            for {
              ref <- Ref.make(List.empty[A])
              iter <- refArrayStream(as*).toZStreamIterator
              _ <- rec(ref, iter, nums.toList)
              list <- ref.get
            } yield assert(list.reverse)(equalTo(exp.toList))
          }
        }

        suite("buildList")(
          makeTest[Int]("empty")()(2, 2, 2)(),
          makeTest("single")(1)(2, 2, 2)(1),
          makeTest("multi")(1, 2, 3, 4, 5)(2, 2, 2)(1, 2, 3, 4, 5),
          makeTest("'num = 0' doesnt close iter")(1, 2, 3, 4, 5)(0, 2)(1, 2),
        )
      }

      suite("takeForeach")(
        buildListSpec,
      )
    }

    suite("zStreamIterator")(
      takeSpec,
      takeForeachSpec,
    )
  }

  override def spec: TestSpec =
    suite("ZIOOpsTests")(
      traverseTests,
      refArrayStreamSpec,
      zStreamIteratorSpec,
    )

}
