package klib

import scala.util.Try

package object scalaCompat {

  trait Implicits {

    implicit class CompatListOps[T](list: List[T]) {

      def partitionMap[A1, A2](f: T => scala.Either[A1, A2]): (List[A1], List[A2]) = {
        val mapped = list.map(f)
        (
          mapped.collect { case scala.Left(a1) => a1 },
          mapped.collect { case scala.Right(a2) => a2 },
        )
      }

    }

    implicit class CompatStringOps(string: String) {

      private def toOptionCompat[T](f: String => T): Option[T] =
        Try(f(string)).toOption

      def toBooleanOption: Option[Boolean] =
        toOptionCompat(_.toBoolean)

      def toIntOption: Option[Int] =
        toOptionCompat(_.toInt)

      def toLongOption: Option[Long] =
        toOptionCompat(_.toLong)

      def toFloatOption: Option[Float] =
        toOptionCompat(_.toFloat)

      def toDoubleOption: Option[Double] =
        toOptionCompat(_.toDouble)

    }

  }
  object Implicits extends Implicits

}
