package klib.unitTests

import org.scalatest.funspec.AnyFunSpec

import klib.Implicits._
import klib.utils._

final class ArrayBufferTests extends AnyFunSpec {

  describe("size") {

    describe("ofInitialSize") {

      it("1") {
        assertResult(0)(ArrayBuffer.ofInitialSize(0).size)
      }

      it("2") {
        assertResult(0)(ArrayBuffer.ofInitialSize(1).size)
      }

      it("3") {
        assertResult(0)(ArrayBuffer.ofInitialSize(2).size)
      }

    }

    describe("wrapFullArray") {

      it("1") {
        assertResult(0)(ArrayBuffer.wrapFullArray(Array[Int]()).size)
      }

      it("2") {
        assertResult(1)(ArrayBuffer.wrapFullArray(Array[Int](0)).size)
      }

      it("3") {
        assertResult(2)(ArrayBuffer.wrapFullArray(Array[Int](0, 1)).size)
      }

    }

  }

  describe("operations") {

    it("1") {
      val wab = ArrayBuffer.ofInitialSize[Int](3)

      assertResult(Array())(wab.toArray)

      wab.append(1)
      assertResult(Array(1))(wab.toArray)

      wab.append(2)
      assertResult(Array(1, 2))(wab.toArray)

      wab.append(3)
      assertResult(Array(1, 2, 3))(wab.toArray)
    }

    it("2") {
      val wab = ArrayBuffer.ofInitialSize[Int](3)

      assertResult(Array())(wab.toArray)

      wab.append(1)
      assertResult(Array(1))(wab.toArray)

      wab.append(2)
      assertResult(Array(1, 2))(wab.toArray)

      wab.append(3)
      assertResult(Array(1, 2, 3))(wab.toArray)

      wab.popHead
      assertResult(Array(2, 3))(wab.toArray)

      wab.append(4)
      assertResult(Array(2, 3, 4))(wab.toArray)

      wab.popHead
      wab.popHead
      assertResult(Array(4))(wab.toArray)

      wab.append(5)
      wab.popHead
      wab.append(6)
      assertResult(Array(5, 6))(wab.toArray)
    }

    it("3") {
      val wab = ArrayBuffer.ofInitialSize[Int](3)

      assertResult(Array())(wab.toArray)

      wab.prepend(3)
      assertResult(Array(3))(wab.toArray)

      wab.prepend(2)
      assertResult(Array(2, 3))(wab.toArray)

      wab.prepend(1)
      assertResult(Array(1, 2, 3))(wab.toArray)

      wab.append(4)
      assertResult(Array(1, 2, 3, 4))(wab.toArray)
    }

  }

  describe("toString") {

    it("1") {
      val wab = ArrayBuffer.of(1, 2, 3)

      assertResult(
        "ArrayBuffer(1, 2, 3)",
      )(
        wab.toString(false),
      )
      assertResult(
        "ArrayBuffer([START], 1, 2, 3)",
      )(
        wab.toString(true),
      )
    }

    it("2") {
      val wab = ArrayBuffer.of(1, 2, 3)
      wab.popHead

      assertResult(
        "ArrayBuffer(2, 3)",
      )(
        wab.toString(false),
      )
      assertResult(
        "ArrayBuffer([EMPTY_SPACE:1], [START], 2, 3)",
      )(
        wab.toString(true),
      )
    }

    it("3") {
      val wab = ArrayBuffer.of(1, 2, 3)
      wab.popLast

      assertResult(
        "ArrayBuffer(1, 2)",
      )(
        wab.toString(false),
      )
      assertResult(
        "ArrayBuffer([START], 1, 2, [EMPTY_SPACE:1])",
      )(
        wab.toString(true),
      )
    }

    it("4") {
      val wab = ArrayBuffer.of(1, 2, 3)
      wab.popHead
      wab.popLast

      assertResult(
        "ArrayBuffer(2)",
      )(
        wab.toString(false),
      )
      assertResult(
        "ArrayBuffer([EMPTY_SPACE:1], [START], 2, [EMPTY_SPACE:1])",
      )(
        wab.toString(true),
      )
    }

  }

  describe("does not modify initialArray") {

    it("1") {
      val array = Array(1, 2, 3)
      val wab = ArrayBuffer.wrapFullArray(array)

      wab.popHead
      assertResult(Array(2, 3))(wab.toArray)
      assertResult(Array(1, 2, 3))(array)
    }

  }

  describe("filterInPlace") {

    it("1") {
      val wab = ArrayBuffer.of(1, 2, 3)
      wab.filterInPlace(_ >= 2)
      assertResult(Array(2, 3))(wab.toArray)
    }

    it("2") {
      val wab = ArrayBuffer.of(0, 1, 2)
      wab.popHead
      wab.append(3)
      wab.filterInPlace(_ >= 2)
      assertResult(Array(2, 3))(wab.toArray)
    }

  }

}
