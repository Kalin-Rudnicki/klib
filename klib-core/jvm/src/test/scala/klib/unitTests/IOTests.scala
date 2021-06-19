package klib.unitTests

import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado

class IOTests extends AnyFunSpec {

  describe("execution-order") {

    def makeTest[T1](
        f1: mutable.ListBuffer[Int] => T1,
    )(
        f2: T1 => Any,
        expSize: Int,
    ): Unit = {
      val lb = mutable.ListBuffer[Int]()
      val t1 = f1(lb)
      assertResult(0, "lb.size")(lb.size)
      f2(t1)
      assertResult(expSize, "lb.size")(lb.size)
      val list = lb.toList
      assertResult(true, "list order")(list == list.sorted)
    }

    describe("IO") {

      def makeIO(lb: mutable.ListBuffer[Int], i: Int): IO[Unit] = {
        lb.append(i)
        ()
      }.pure[IO]

      it("1") {
        makeTest { lb =>
          makeIO(lb, 1)
        }(_.runSync, 1)
      }

      it("2") {
        makeTest { lb =>
          ado[IO].join(
            makeIO(lb, 1),
            makeIO(lb, 2),
          )
        }(_.runSync, 2)
      }

      it("3") {
        makeTest { lb =>
          ado[IO].join(
            makeIO(lb, 1),
            makeIO(lb, 2),
            makeIO(lb, 3),
          )
        }(_.runSync, 3)
      }

    }

    describe("??") {

      def make_??(lb: mutable.ListBuffer[Int], i: Int): ??[Unit] = {
        lb.append(i)
        ()
      }.pure[??]

      it("1") {
        makeTest { lb =>
          make_??(lb, 1)
        }(_.runSync, 1)
      }

      it("2") {
        makeTest { lb =>
          ado[??].join(
            make_??(lb, 1),
            make_??(lb, 2),
          )
        }(_.runSync, 2)
      }

      it("3") {
        makeTest { lb =>
          ado[??].join(
            make_??(lb, 1),
            make_??(lb, 2),
            make_??(lb, 3),
          )
        }(_.runSync, 3)
      }

    }

  }

}
