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

      def minOption(implicit ordering: Ordering[T]): scala.Option[T] =
        list match {
          case Nil => scala.None
          case _ => scala.Some(list.min)
        }

      def maxOption(implicit ordering: Ordering[T]): scala.Option[T] =
        list match {
          case Nil => scala.None
          case _ => scala.Some(list.max)
        }

      def groupMap[K, B](k: T => K)(b: T => B): Map[K, List[B]] =
        list.groupBy(k).map { case (k, v) => k -> v.map(b) }

      def appended(t: T): List[T] =
        (t :: list.reverse).reverse

    }

    implicit class CompatSetOps[T](set: Set[T]) {

      def partitionMap[A1, A2](f: T => scala.Either[A1, A2]): (Set[A1], Set[A2]) = {
        val (left, right) = set.toList.partitionMap(f)
        (left.toSet, right.toSet)
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
