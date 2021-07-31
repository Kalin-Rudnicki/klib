package klib.utils

import scala.reflect.ClassTag

import klib.Implicits._
import klib.fp.types._

/**
  * Write access limited to head/last.
  * Read access of entire Array.
  * Never needs to copy unless size grows above max.
  * (aka: efficient usage for fixed size queue)
  */
final class ArrayBuffer[T: ClassTag] private (
    initialArray: Array[T],
    initialOffset: Int,
    initialSize: Int,
) {

  private var _array: Array[T] = initialArray
  private var _offset: Int = initialOffset
  private var _size: Int = initialSize

  def arrayLength: Int =
    _array.length

  def size: Int =
    _size

  def offset: Int =
    _offset

  // =====|  |=====

  private def actualIdx(idx: Int): Maybe[Int] =
    (idx >= 0 && idx < _size).maybe {
      (idx + _offset) % _array.length
    }

  private def elemAllocation: (Int, Int) = {
    val elemsAtEnd = (_array.length - _offset).min(_size)
    val elemsAtStart = _size - elemsAtEnd

    (elemsAtEnd, elemsAtStart)
  }

  // =====|  |=====

  def apply(idx: Int): Maybe[T] =
    actualIdx(idx).map(_array)

  def head: Maybe[T] =
    apply(0)

  def last: Maybe[T] =
    apply(_size - 1)

  def popHead: Maybe[T] =
    actualIdx(0).map { idx =>
      val elem = _array(idx)
      _array(idx) = null.asInstanceOf[T]
      _offset = (_offset + 1) % _array.length
      _size -= 1
      elem
    }

  def popLast: Maybe[T] =
    actualIdx(_size - 1).map { idx =>
      val elem = _array(idx)
      _array(idx) = null.asInstanceOf[T]
      _size -= 1
      elem
    }

  private def growIfFull(): Unit =
    if (_size == _array.length) {
      _array = toArray((_array.length * 2).max(1))
      _offset = 0
    }

  def prepend(elem: T): Unit = {
    growIfFull()

    _offset = (_offset - 1 + _array.length) % _array.length
    _size += 1
    _array(_offset) = elem
  }

  def append(elem: T): Unit = {
    growIfFull()

    _array((_size + _offset) % _array.length) = elem
    _size += 1
  }

  private def toArray(newSize: Int): Array[T] = {
    val resArray = new Array[T](newSize)
    val (elemsAtEnd, elemsAtStart) = elemAllocation
    Array.copy(_array, _offset, resArray, 0, elemsAtEnd)
    Array.copy(_array, 0, resArray, elemsAtEnd, elemsAtStart)
    resArray
  }

  def toArray: Array[T] =
    toArray(_size)

  def toString(showInternalOrder: Boolean): String = {
    val stringBuilder = new StringBuilder
    var first = true
    def append(any: Any): Unit = {
      if (first)
        first = false
      else
        stringBuilder.append(", ")
      stringBuilder.append(if (any == null) "null" else any.toString)
    }

    stringBuilder.append("ArrayBuffer(")

    val (elemsAtEnd, elemsAtStart) = elemAllocation
    if (showInternalOrder) {
      0.until(elemsAtStart).foreach { i =>
        append(_array(i))
      }

      if (elemsAtStart < _offset)
        append(s"[EMPTY_SPACE:${_offset - elemsAtStart}]")

      append("[START]")

      0.until(elemsAtEnd).foreach { i =>
        append(_array(i + _offset))
      }

      if (_offset + _size < _array.length)
        append(s"[EMPTY_SPACE:${_array.length - (_offset + _size)}]")

    } else {
      0.until(elemsAtEnd).foreach { i =>
        append(_array(i + _offset))
      }

      0.until(elemsAtStart).foreach { i =>
        append(_array(i))
      }
    }

    stringBuilder.append(")")

    stringBuilder.toString
  }

  override def toString: String =
    toString(false)

}

object ArrayBuffer {

  def ofInitialSize[T: ClassTag](size: Int): ArrayBuffer[T] =
    new ArrayBuffer[T](
      initialArray = new Array[T](size.max(0)),
      initialOffset = 0,
      initialSize = 0,
    )

  def of[T: ClassTag](elems: T*): ArrayBuffer[T] = {
    val elemArray = elems.toArray
    new ArrayBuffer[T](
      initialArray = elemArray,
      initialOffset = 0,
      initialSize = elemArray.length,
    )
  }

  def wrapFullArray[T: ClassTag](array: Array[T]): ArrayBuffer[T] = {
    val dupeArray = array.clone

    new ArrayBuffer[T](
      initialArray = dupeArray,
      initialOffset = 0,
      initialSize = dupeArray.length,
    )
  }

}
