package klib.utils.commandLine.parse

import cats.syntax.option.*

sealed trait DefaultableOption[+V] {

  def isAuto: Boolean =
    this match {
      case Defaultable.Auto        => true
      case Defaultable.Some(value) => false
      case Defaultable.None        => false
    }

  def toOptionV[V2 >: V](default: => V2): Option[V2] =
    this match {
      case Defaultable.Auto        => default.some
      case Defaultable.Some(value) => value.some
      case Defaultable.None        => None
    }

  def toOptionO[V2 >: V](default: => Option[V2]): Option[V2] =
    this match {
      case Defaultable.Auto        => default
      case Defaultable.Some(value) => value.some
      case Defaultable.None        => None
    }

}
sealed trait Defaultable[+V] extends DefaultableOption[V] {

  def toValue[V2 >: V](default: => V2): V2 =
    this match {
      case Defaultable.Auto        => default
      case Defaultable.Some(value) => value
    }

}
object Defaultable {
  case object Auto extends Defaultable[Nothing]
  final case class Some[+V](value: V) extends Defaultable[V]
  case object None extends DefaultableOption[Nothing]
}
