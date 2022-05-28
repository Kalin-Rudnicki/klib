package klib.utils

import cats.syntax.either.*
import cats.syntax.option.*
import scala.reflect.ClassTag
import zio.*

/**
  * Write access limited to head/last.
  * Read access of entire Array.
  * Never needs to copy unless size grows above max. (aka: efficient usage for fixed size queue)
  */
final class ArrayBuffer[T: ClassTag] private (ref: Ref[ArrayBuffer.State[T]]) {

  // =====| private |=====

  private def readUIO[T2](
      readF: ArrayBuffer.State[T] => T2,
  ): UIO[T2] =
    ref.get.map(readF)

  private def readTaskM[T2](
      readF: ArrayBuffer.State[T] => EitherError[T2],
  ): KTask[T2] =
    ref.get.flatMap(state => ZIO.fromEither(readF(state)).toErrorNEL)

  private def writeUIO[T2](
      writeF: ArrayBuffer.State[T] => (T2, ArrayBuffer.State[T]),
  ): UIO[T2] =
    ref.modify(writeF)

  private def writeTaskM[T2](
      writeF: ArrayBuffer.State[T] => EitherError[(T2, ArrayBuffer.State[T])],
  ): KTask[T2] =
    ref
      .modify { state =>
        writeF(state) match {
          case Left(message)       => (message.asLeft, state)
          case Right((t2, state2)) => (t2.asRight, state2)
        }
      }
      .flatMap(ZIO.fromEither(_).toErrorNEL)

  // =====| misc |=====

  def size: UIO[Int] =
    readUIO(_.size)

  // TODO : REMOVE

  def getRawArray: UIO[Array[T]] =
    ref.get.map(_.array)

  // =====| get |=====

  def apply(idx: Int): KTask[T] =
    readTaskM(_.apply(idx))

  def get(idx: Int): UIO[Option[T]] =
    readUIO(_.get(idx))

  def head: KTask[T] =
    readTaskM(_.head)

  def headOption: UIO[Option[T]] =
    readUIO(_.headOption)

  def last: KTask[T] =
    readTaskM(_.last)

  def lastOption: UIO[Option[T]] =
    readUIO(_.lastOption)

  // =====| pop |=====

  def popHead: KTask[T] =
    writeTaskM(_.popHead)

  def popHeadOption: UIO[Option[T]] =
    writeUIO(_.popHeadOption)

  def popLast: KTask[T] =
    writeTaskM(_.popLast)

  def popLastOption: UIO[Option[T]] =
    writeUIO(_.popLastOption)

  // =====| insert |=====

  def prepend(elem: T): UIO[Unit] =
    writeUIO(_.prepend(elem))

  def prependFixed(elem: T): KTask[Option[T]] =
    writeTaskM(_.prependFixed(elem))

  def append(elem: T): UIO[Unit] =
    writeUIO(_.append(elem))

  def appendFixed(elem: T): KTask[Option[T]] =
    writeTaskM(_.appendFixed(elem))

  // =====| _____InPlace |=====

  def filterInPlace(p: T => Boolean): UIO[Unit] =
    writeUIO(_.filterInPlace(p))

  def removedFromFilterInPlace(p: T => Boolean): UIO[List[T]] =
    writeUIO(_.removedFromFilterInPlace(p))

  def mapInPlace(f: T => T): UIO[Unit] =
    writeUIO(_.mapInPlace(f))

  def distinctInPlaceBy[T2](f: T => T2): UIO[Unit] =
    writeUIO(_.distinctInPlaceBy(f))

  def distinctInPlace: UIO[Unit] =
    writeUIO(_.distinctInPlace)

  // =====| to_____ |=====

  def toArray: UIO[Array[T]] =
    readUIO(_.toArray)

  def filterToList(p: T => Boolean): UIO[List[T]] =
    readUIO(_.filterToList(p))

  def mapToList[T2](f: T => T2): UIO[List[T2]] =
    readUIO(_.mapToList(f))

  // =====| mkString |=====

  def mkString(showInternalOrder: Boolean): UIO[String] =
    readUIO(_.mkString(showInternalOrder))

}

object ArrayBuffer {

  private final case class State[T: ClassTag](
      array: Array[T],
      offset: Int,
      size: Int,
  ) {

    // =====| private |=====

    private type ReadUIO[T2] = T2
    private type ReadTaskM[T2] = EitherError[T2]
    private type WriteUIO[T2] = (T2, State[T])
    private type WriteTaskM[T2] = EitherError[(T2, State[T])]

    private def unsafeActualIndex(idx: Int): Int =
      (idx + offset) % array.length

    private def actualIndex(idx: Int): Option[Int] =
      Option.when(idx >= 0 && idx < size)(unsafeActualIndex(idx))

    private def elemAllocation: (Int, Int) = {
      val elemsAtEnd = (array.length - offset).min(size)
      val elemsAtStart = size - elemsAtEnd
      (elemsAtEnd, elemsAtStart)
    }

    private def readUIOToTaskM[T2](uio: ReadUIO[Option[T2]], error: => KError): ReadTaskM[T2] =
      uio match {
        case Some(res) => res.asRight
        case None      => error.asLeft
      }

    private def writeUIOToTaskM[T2](uio: WriteUIO[Option[T2]], error: => KError): WriteTaskM[T2] =
      uio match {
        case (res, state) =>
          res match {
            case Some(res) => (res, state).asRight
            case None      => error.asLeft
          }
      }

    // =====| get |=====

    def apply(idx: Int): ReadTaskM[T] =
      readUIOToTaskM(get(idx), KError.Unexpected(s"Index out of bounds: $idx"))

    def get(idx: Int): ReadUIO[Option[T]] =
      actualIndex(idx).map(array(_))

    def head: ReadTaskM[T] =
      apply(0)

    def headOption: ReadUIO[Option[T]] =
      get(0)

    def last: ReadTaskM[T] =
      apply(size - 1)

    def lastOption: ReadUIO[Option[T]] =
      get(size - 1)

    // =====| pop |=====

    def popHead: WriteTaskM[T] =
      writeUIOToTaskM(popHeadOption, KError.Unexpected("No head to pop in ArrayBuffer"))

    def popHeadOption: WriteUIO[Option[T]] =
      actualIndex(0) match {
        case Some(idx) =>
          val elem = array(idx)
          array(idx) = null.asInstanceOf[T]
          (elem.some, copy(offset = (offset + 1) % array.length, size = size - 1))
        case None =>
          (None, this)
      }

    def popLast: WriteTaskM[T] =
      writeUIOToTaskM(popLastOption, KError.Unexpected("No last to pop in ArrayBuffer"))

    def popLastOption: WriteUIO[Option[T]] =
      actualIndex(size - 1) match {
        case Some(idx) =>
          val elem = array(idx)
          array(idx) = null.asInstanceOf[T]
          (elem.some, copy(size = size - 1))
        case None =>
          (None, this)
      }

    // =====| insert |=====

    private def growIfFull: WriteUIO[Unit] =
      if (size < array.length) ((), this)
      else
        ((), copy(array = toArray((array.length * 2).max(1)), offset = 0))

    def prepend(elem: T): WriteUIO[Unit] = {
      val (_, newState) = growIfFull
      val newOffset = (newState.offset - 1 + newState.array.length) % newState.array.length
      newState.array(newOffset) = elem
      ((), newState.copy(offset = newOffset, size = newState.size + 1))
    }

    def prependFixed(elem: T): WriteTaskM[Option[T]] =
      if (array.length == 0) KError.Unexpected("Can not run 'prependFixed' on an 0-length array").asLeft
      else if (size < array.length) {
        val (_, newState) = prepend(elem)
        (None, newState).asRight
      } else
        for {
          (last, newState) <- popLast
          (_, newState2) = newState.prepend(elem)
        } yield (last.some, newState2)

    def append(elem: T): WriteUIO[Unit] = {
      val (_, newState) = growIfFull
      newState.array((newState.size + newState.offset) % newState.array.length) = elem
      ((), newState.copy(size = newState.size + 1))
    }

    def appendFixed(elem: T): WriteTaskM[Option[T]] =
      if (array.length == 0) KError.Unexpected("Can not run 'appendFixed' on an 0-length array").asLeft
      else if (size < array.length) {
        val (_, newState) = append(elem)
        (None, newState).asRight
      } else
        for {
          (head, newState) <- popHead
          (_, newState2) = newState.append(elem)
        } yield (head.some, newState2)

    // =====| _____InPlace |=====

    def filterInPlace(p: T => Boolean): WriteUIO[Unit] = {
      var delta = 0
      0.until(size).foreach { idx =>
        val aIdx = unsafeActualIndex(idx)
        val elem = array(aIdx)
        if (p(elem)) {
          if (delta != 0) {
            array(unsafeActualIndex(idx - delta)) = elem
            array(aIdx) = null.asInstanceOf[T]
          }
        } else {
          array(aIdx) = null.asInstanceOf[T]
          delta += 1
        }
      }
      ((), copy(size = size - delta))
    }

    def removedFromFilterInPlace(p: T => Boolean): WriteUIO[List[T]] = {
      var list = List.empty[T]
      var delta = 0
      0.until(size).foreach { idx =>
        val aIdx = unsafeActualIndex(idx)
        val elem = array(aIdx)
        if (p(elem)) {
          if (delta != 0) {
            array(unsafeActualIndex(idx - delta)) = elem
            array(aIdx) = null.asInstanceOf[T]
          }
        } else {
          list = elem :: list
          array(aIdx) = null.asInstanceOf[T]
          delta += 1
        }
      }
      (list.reverse, copy(size = size - delta))
    }

    def mapInPlace(f: T => T): WriteUIO[Unit] = {
      0.until(size).foreach { idx =>
        val aIdx = unsafeActualIndex(idx)
        array(aIdx) = f(array(aIdx))
      }
      ((), this)
    }

    def distinctInPlaceBy[T2](f: T => T2): WriteUIO[Unit] = {
      val seen = scala.collection.mutable.Set[T2]()
      filterInPlace { elem =>
        val mapped = f(elem)
        if (seen.contains(mapped)) false
        else {
          seen.add(mapped)
          true
        }
      }
    }

    def distinctInPlace: WriteUIO[Unit] =
      distinctInPlaceBy(identity)

    // =====| to_____ |=====

    private def toArray(newSize: Int): ReadUIO[Array[T]] = {
      val resArray = new Array[T](newSize)
      def copyArray(srcPos: Int, destPos: Int, length: Int): Unit =
        Array.copy(array, srcPos, resArray, destPos, length)

      val (elemsAtEnd, elemsAtStart) = elemAllocation

      copyArray(offset, 0, elemsAtEnd)
      copyArray(0, elemsAtEnd, elemsAtStart)
      resArray
    }

    def toArray: ReadUIO[Array[T]] =
      toArray(size)

    def filterToList(p: T => Boolean): ReadUIO[List[T]] = {
      var list = List.empty[T]
      0.until(size).reverse.foreach { idx =>
        val aIdx = unsafeActualIndex(idx)
        val elem = array(aIdx)
        if (p(elem))
          list = elem :: list
      }
      list
    }

    def mapToList[T2](f: T => T2): ReadUIO[List[T2]] = {
      var list = List.empty[T2]
      0.until(size).reverse.foreach { idx =>
        list = f(array(unsafeActualIndex(idx))) :: list
      }
      list
    }

    // =====| mkString |=====

    def mkString(showInternalOrder: Boolean): ReadUIO[String] = {
      val stringBuilder = new StringBuilder
      var first = true
      def append(any: Any): Unit = {
        if (first)
          first = false
        else
          stringBuilder.append(", ")
        stringBuilder.append(if (any == null) "null" else any.toString)
      }

      val (elemsAtEnd, elemsAtStart) = elemAllocation
      if (showInternalOrder) {
        stringBuilder.append(s"ArrayBuffer<$offset, $size, ${array.length}>(")

        0.until(elemsAtStart).foreach { i =>
          append(array(i))
        }

        if (elemsAtStart < offset)
          append(s"[EMPTY_SPACE:${offset - elemsAtStart}]")

        append("[START]")

        0.until(elemsAtEnd).foreach { i =>
          append(array(i + offset))
        }

        if (offset + size < array.length)
          append(s"[EMPTY_SPACE:${array.length - (offset + size)}]")

      } else {
        stringBuilder.append("ArrayBuffer(")

        0.until(elemsAtEnd).foreach { i =>
          append(array(i + offset))
        }

        0.until(elemsAtStart).foreach { i =>
          append(array(i))
        }
      }

      stringBuilder.append(")")

      stringBuilder.toString
    }

    override def toString: String =
      mkString(true)

  }

  def ofInitialSize[T: ClassTag](size: Int): UIO[ArrayBuffer[T]] =
    Ref.make(State(new Array[T](size.max(0)), 0, 0)).map(ArrayBuffer(_))

  def of[T: ClassTag](elems: T*): UIO[ArrayBuffer[T]] = {
    val elemArray = elems.toArray
    Ref.make(State(elemArray, 0, elemArray.length)).map(ArrayBuffer(_))
  }

  def wrapFullArray[T: ClassTag](array: Array[T]): UIO[ArrayBuffer[T]] = {
    val dupeArray = array.clone
    Ref.make(State(dupeArray, 0, dupeArray.length)).map(ArrayBuffer(_))
  }

}
