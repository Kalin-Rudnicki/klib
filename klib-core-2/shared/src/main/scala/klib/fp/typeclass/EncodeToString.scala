package klib.fp.typeclass

import java.util.UUID

import io.circe.*
import io.circe.syntax.*

// =====| Core |=====

trait EncodeToString[-T] {

  def encode(t: T): String

  final def map[T2](f: T2 => T): EncodeToString[T2] =
    t2 => encode(f(t2))

}
object EncodeToString {

  def apply[T: EncodeToString]: EncodeToString[T] =
    implicitly[EncodeToString[T]]

  def fromCirceEncoder[T: Encoder](toString: Json => String): EncodeToString[T] =
    t => toString(t.asJson)

  def fromCirceEncoder[T: Encoder]: EncodeToString[T] =
    fromCirceEncoder[T]((json: Json) => json.noSpaces)

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
