package klib.unitTest.utils

import cats.data.NonEmptyList
import cats.syntax.option.*
import scala.reflect.ClassTag
import zio.{test as _, *}
import zio.test.*
import zio.test.Assertion.*

import klib.unitTest.SpecUtils.*
import klib.utils.*

object ArrayBufferTests extends DefaultKSpec {

  val sizeSpec: TestSpec = {
    def makeTest[T](name: String)(make: UIO[ArrayBuffer[T]])(expSize: Int): TestSpec =
      test(name) {
        for {
          arrayBuffer <- make
          size <- arrayBuffer.size
        } yield assert(size)(equalTo(expSize))
      }

    suite("size")(
      suite("ofInitialSize")(
        makeTest("0")(ArrayBuffer.ofInitialSize[Int](0))(0),
        makeTest("1")(ArrayBuffer.ofInitialSize[Int](1))(0),
        makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(0),
      ),
      suite("of")(
        makeTest("0")(ArrayBuffer.of[Int]())(0),
        makeTest("1")(ArrayBuffer.of[Int](0))(1),
        makeTest("2")(ArrayBuffer.of[Int](0, 1))(2),
      ),
      suite("wrapFullArray")(
        makeTest("0")(ArrayBuffer.wrapFullArray(Array[Int]()))(0),
        makeTest("1")(ArrayBuffer.wrapFullArray(Array[Int](0)))(1),
        makeTest("2")(ArrayBuffer.wrapFullArray(Array[Int](0, 1)))(2),
      ),
    )
  }

  val operationsSpec: TestSpec = {
    final case class Op[T, T2](op: ArrayBuffer[T] => TaskM[T2], opExp: T2, arrayExp: Array[T]) {

      def toResult(idx: Int, arrayBuffer: ArrayBuffer[T]): TaskM[TestResult] =
        for {
          res <- op(arrayBuffer)
          array <- arrayBuffer.toArray
          resResult = assert(res)(equalTo(opExp).imap(s"res[$idx]", identity))
          arrayResult = assert(array.toList)(equalTo(arrayExp.toList).imap(s"array[$idx]", identity))
        } yield resResult && arrayResult

    }

    def makeTest[T](name: String)(make: UIO[ArrayBuffer[T]])(ops: Op[T, _]*): TestSpec =
      test(name) {
        def rec(idx: Int, arrayBuffer: ArrayBuffer[T], ops: List[Op[T, _]]): RIOM[Logger, TestResult] =
          ops match {
            case op :: tail =>
              for {
                beforeStr <- arrayBuffer.mkString(true)
                myResult <- op.toResult(idx, arrayBuffer)
                afterStr <- arrayBuffer.mkString(true)
                _ <- Logger.println.debug(s"[$idx] : $beforeStr => $afterStr")
                tailResult <- rec(idx + 1, arrayBuffer, tail)
              } yield myResult && tailResult
            case Nil =>
              ZIO.succeed(assertCompletes)
          }

        Logger.println.debug(name) *>
          make.flatMap(rec(0, _, ops.toList))
      }

    suite("operations")(
      suite("append")(
        suite("popHead")(
          makeTest("1")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.append(1), (), Array(1)),
            Op(_.popHeadOption, 1.some, Array()),
            Op(_.popHeadOption, None, Array()),
          ),
          makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.append(1), (), Array(1)),
            Op(_.append(2), (), Array(1, 2)),
            Op(_.append(3), (), Array(1, 2, 3)),
            Op(_.popHeadOption, 1.some, Array(2, 3)),
            Op(_.popHeadOption, 2.some, Array(3)),
            Op(_.popHeadOption, 3.some, Array()),
            Op(_.popHeadOption, None, Array()),
          ),
        ),
        suite("popLast")(
          makeTest("1")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.append(1), (), Array(1)),
            Op(_.popLastOption, 1.some, Array()),
            Op(_.popLastOption, None, Array()),
          ),
          makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.append(1), (), Array(1)),
            Op(_.append(2), (), Array(1, 2)),
            Op(_.append(3), (), Array(1, 2, 3)),
            Op(_.popLastOption, 3.some, Array(1, 2)),
            Op(_.popLastOption, 2.some, Array(1)),
            Op(_.popLastOption, 1.some, Array()),
            Op(_.popLastOption, None, Array()),
          ),
        ),
      ),
      suite("prepend")(
        suite("popHead")(
          makeTest("1")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.prepend(1), (), Array(1)),
            Op(_.popHeadOption, 1.some, Array()),
            Op(_.popHeadOption, None, Array()),
          ),
          makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.prepend(3), (), Array(3)),
            Op(_.prepend(2), (), Array(2, 3)),
            Op(_.prepend(1), (), Array(1, 2, 3)),
            Op(_.popHeadOption, 1.some, Array(2, 3)),
            Op(_.popHeadOption, 2.some, Array(3)),
            Op(_.popHeadOption, 3.some, Array()),
            Op(_.popHeadOption, None, Array()),
          ),
        ),
        suite("popLast")(
          makeTest("1")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.prepend(1), (), Array(1)),
            Op(_.popLastOption, 1.some, Array()),
            Op(_.popLastOption, None, Array()),
          ),
          makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.prepend(3), (), Array(3)),
            Op(_.prepend(2), (), Array(2, 3)),
            Op(_.prepend(1), (), Array(1, 2, 3)),
            Op(_.popLastOption, 3.some, Array(1, 2)),
            Op(_.popLastOption, 2.some, Array(1)),
            Op(_.popLastOption, 1.some, Array()),
            Op(_.popLastOption, None, Array()),
          ),
        ),
      ),
      suite("appendFixed")(
        suite("popHead")(
          makeTest("1")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.appendFixed(1), None, Array(1)),
            Op(_.popHeadOption, 1.some, Array()),
            Op(_.popHeadOption, None, Array()),
          ),
          makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.appendFixed(1), None, Array(1)),
            Op(_.appendFixed(2), None, Array(1, 2)),
            Op(_.appendFixed(3), 1.some, Array(2, 3)),
            Op(_.popHeadOption, 2.some, Array(3)),
            Op(_.popHeadOption, 3.some, Array()),
            Op(_.popHeadOption, None, Array()),
            Op(_.popHeadOption, None, Array()),
          ),
        ),
        suite("popLast")(
          makeTest("1")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.appendFixed(1), None, Array(1)),
            Op(_.popLastOption, 1.some, Array()),
            Op(_.popLastOption, None, Array()),
          ),
          makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.appendFixed(1), None, Array(1)),
            Op(_.appendFixed(2), None, Array(1, 2)),
            Op(_.appendFixed(3), 1.some, Array(2, 3)),
            Op(_.popLastOption, 3.some, Array(2)),
            Op(_.popLastOption, 2.some, Array()),
            Op(_.popLastOption, None, Array()),
            Op(_.popLastOption, None, Array()),
          ),
        ),
      ),
      suite("prependFixed")(
        suite("popHead")(
          makeTest("1")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.prependFixed(1), None, Array(1)),
            Op(_.popHeadOption, 1.some, Array()),
            Op(_.popHeadOption, None, Array()),
          ),
          makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.prependFixed(3), None, Array(3)),
            Op(_.prependFixed(2), None, Array(2, 3)),
            Op(_.prependFixed(1), 3.some, Array(1, 2)),
            Op(_.popHeadOption, 1.some, Array(2)),
            Op(_.popHeadOption, 2.some, Array()),
            Op(_.popHeadOption, None, Array()),
            Op(_.popHeadOption, None, Array()),
          ),
        ),
        suite("popLast")(
          makeTest("1")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.prependFixed(1), None, Array(1)),
            Op(_.popLastOption, 1.some, Array()),
            Op(_.popLastOption, None, Array()),
          ),
          makeTest("2")(ArrayBuffer.ofInitialSize[Int](2))(
            Op(_.prependFixed(3), None, Array(3)),
            Op(_.prependFixed(2), None, Array(2, 3)),
            Op(_.prependFixed(1), 3.some, Array(1, 2)),
            Op(_.popLastOption, 2.some, Array(1)),
            Op(_.popLastOption, 1.some, Array()),
            Op(_.popLastOption, None, Array()),
            Op(_.popLastOption, None, Array()),
          ),
        ),
      ),
    ) @@ TestAspect.sequential
  }

  override def spec: TestSpec =
    suite("ArrayBufferTests")(
      sizeSpec,
      operationsSpec,
    )

}
