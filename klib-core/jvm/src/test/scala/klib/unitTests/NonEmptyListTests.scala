package klib.unitTests

import scala.annotation.tailrec

import org.scalatest.funspec.AnyFunSpec

import klib.extensions.{given, _}
import klib.instances.{given, _}
import klib.fp.types._

class NonEmptyListTests extends AnyFunSpec {

  private val list0: List[Int] = List()
  private val list1: List[Int] = List(1)
  private val list2: List[Int] = List(1, 2)
  private val list3: List[Int] = List(1, 2, 3)

  private val neList1: NonEmptyList[Int] = NonEmptyList.nel(1)
  private val neList2: NonEmptyList[Int] = NonEmptyList.nel(1, 2)
  private val neList3: NonEmptyList[Int] = NonEmptyList.nel(1, 2, 3)

  describe("builders") {

    describe("nel") {

      it("1") {
        assertResult(NonEmptyList(1, Nil))(NonEmptyList.nel(1))
      }

      it("2") {
        assertResult(NonEmptyList(1, List(2)))(NonEmptyList.nel(1, 2))
      }

      it("3") {
        assertResult(NonEmptyList(1, List(2, 3)))(NonEmptyList.nel(1, 2, 3))
      }

    }

    describe("toNel") {

      it("1") {
        assertResult(None)(list0.toNel)
      }

      it("2") {
        assertResult(neList1.some)(list1.toNel)
      }

      it("3") {
        assertResult(neList2.some)(list2.toNel)
      }

      it("4") {
        assertResult(neList3.some)(list3.toNel)
      }

    }

  }

  describe("operations") {

    describe("toList") {

      it("1") {
        assertResult(list1)(neList1.toList)
      }

      it("2") {
        assertResult(list2)(neList2.toList)
      }

      it("3") {
        assertResult(list3)(neList3.toList)
      }

    }

    describe("zipWithIndex") {

      it("1") {
        assertResult(NonEmptyList.nel((1, 0)))(neList1.zipWithIndex)
      }

      it("2") {
        assertResult(NonEmptyList.nel((1, 0), (2, 1)))(neList2.zipWithIndex)
      }

      it("3") {
        assertResult(NonEmptyList.nel((1, 0), (2, 1), (3, 2)))(neList3.zipWithIndex)
      }

    }

    describe("::") {

      it("1") {
        assertResult(NonEmptyList.nel(0, 1))(0 :: neList1)
      }

      it("2") {
        assertResult(NonEmptyList.nel(0, 1, 2))(0 :: neList2)
      }

      it("3") {
        assertResult(NonEmptyList.nel(0, 1, 2, 3))(0 :: neList3)
      }

    }

    describe(":::") {

      describe("empty-list") {

        it("1") {
          assertResult(neList1)(Nil ::: neList1)
        }

        it("2") {
          assertResult(neList2)(Nil ::: neList2)
        }

        it("3") {
          assertResult(neList3)(Nil ::: neList3)
        }

      }

      describe("self-List") {

        it("1") {
          assertResult(NonEmptyList.nel(1, 1))(list1 ::: neList1)
        }

        it("2") {
          assertResult(NonEmptyList.nel(1, 2, 1, 2))(list2 ::: neList2)
        }

        it("3") {
          assertResult(NonEmptyList.nel(1, 2, 3, 1, 2, 3))(list3 ::: neList3)
        }

      }

      describe("self-NonEmptyList") {

        it("1") {
          assertResult(NonEmptyList.nel(1, 1))(neList1 ::: neList1)
        }

        it("2") {
          assertResult(NonEmptyList.nel(1, 2, 1, 2))(neList2 ::: neList2)
        }

        it("3") {
          assertResult(NonEmptyList.nel(1, 2, 3, 1, 2, 3))(neList3 ::: neList3)
        }

      }

    }

    describe("reverse") {

      it("1") {
        assertResult(NonEmptyList.nel(1))(neList1.reverse)
      }

      it("2") {
        assertResult(NonEmptyList.nel(2, 1))(neList2.reverse)
      }

      it("3") {
        assertResult(NonEmptyList.nel(3, 2, 1))(neList3.reverse)
      }

    }

  }

  describe("Monad") {

    val mList0: NonEmptyList[Int] = NonEmptyList.nel(0, 1, 2, 3)
    val mList1: NonEmptyList[Int] = NonEmptyList.nel(1, 2, 3)
    val mList2: NonEmptyList[String] = NonEmptyList.nel("A", "BB", "CCC")

    describe("map") {

      it("1") {
        assertResult(NonEmptyList.nel(2, 3, 4))(mList1.map(_ + 1))
      }

      it("2") {
        assertResult(NonEmptyList.nel("a", "bb", "ccc"))(mList2.map(_.toLowerCase))
      }

    }

    describe("pure") {

      it("1") {
        assertResult(NonEmptyList.nel(1))(1.pure[NonEmptyList])
      }

      it("2") {
        assertResult(NonEmptyList.nel("A"))("A".pure[NonEmptyList])
      }

    }

    describe("apply") {

      it("1") {
        assertResult(NonEmptyList.nel(2, 3, 4, 3, 4, 5))(mList1.apply(NonEmptyList.nel[Int => Int](_ + 1, _ + 2)))
      }

    }

    describe("flatMap") {
      def intToNEListInt(i: Int): NonEmptyList[Int] = {
        @tailrec
        def loop(
            remaining: Int,
            stack: NonEmptyList[Int],
        ): NonEmptyList[Int] =
          if (remaining > 0)
            loop(
              remaining - 1,
              i :: stack,
            )
          else
            stack

        if (i > 0)
          loop(
            i - 1,
            NonEmptyList(i, Nil),
          )
        else
          NonEmptyList(0, Nil)
      }

      it("1") {
        assertResult(NonEmptyList.nel(0, 1, 2, 2, 3, 3, 3))(mList0.flatMap(intToNEListInt))
      }

      it("2") {
        assertResult(NonEmptyList.nel(1, 2, 2, 3, 3, 3))(mList1.flatMap(intToNEListInt))
      }

    }

  }

}
