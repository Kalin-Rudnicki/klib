package klib.fp.typeclass

import java.util.UUID

trait EncodeToString[-T] {
  def encode(t: T): String
}
object EncodeToString {

  trait Implicits {

    implicit class ESIdOps[T](t: T) {

      def encodeToString(implicit encode: EncodeToString[T]): String =
        encode.encode(t)

    }

  }
  object Implicits extends Implicits

  def usingToString[T]: EncodeToString[T] = _.toString

  implicit val encodeStringToString: EncodeToString[String] = identity(_)

  implicit val encodeBooleanToString: EncodeToString[Boolean] = usingToString

  implicit val encodeIntToString: EncodeToString[Int] = usingToString

  implicit val encodeLongToString: EncodeToString[Long] = usingToString

  implicit val encodeFloatToString: EncodeToString[Float] = usingToString

  implicit val encodeDoubleToString: EncodeToString[Double] = usingToString

  implicit def encodeListToString[T](implicit encodeRToString: EncodeToString[T]): EncodeToString[List[T]] =
    _.map(encodeRToString.encode).mkString(",")

  implicit val encodeUUIDToString: EncodeToString[UUID] = usingToString

}
