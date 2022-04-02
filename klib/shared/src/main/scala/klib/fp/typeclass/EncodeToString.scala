package klib.fp.typeclass

import java.util.UUID
import zio.json.*

// =====| Core |=====

trait EncodeToString[-T] {

  def encode(t: T): String

  final def map[T2](f: T2 => T): EncodeToString[T2] =
    t2 => encode(f(t2))

}
object EncodeToString {

  def apply[T: EncodeToString]: EncodeToString[T] =
    implicitly[EncodeToString[T]]

  def fromJsonEncoder[T: JsonEncoder](spaces: Option[Int]): EncodeToString[T] =
    JsonEncoder[T].encodeJson(_, spaces).toString

  def fromJsonEncoder[T: JsonEncoder]: EncodeToString[T] =
    JsonEncoder[T].encodeJson(_, None).toString

  def usingToString[T]: EncodeToString[T] = _.toString

  implicit val encodeStringToString: EncodeToString[String] = identity(_)

  implicit val encodeBooleanToString: EncodeToString[Boolean] = usingToString

  implicit val encodeIntToString: EncodeToString[Int] = usingToString

  implicit val encodeLongToString: EncodeToString[Long] = usingToString

  implicit val encodeFloatToString: EncodeToString[Float] = usingToString

  implicit val encodeDoubleToString: EncodeToString[Double] = usingToString

  implicit val encodeUUIDToString: EncodeToString[UUID] = usingToString

}

// =====| Extensions |=====

extension [T](t: T) {
  def encodeToString(implicit ets: EncodeToString[T]): String =
    ets.encode(t)
}
