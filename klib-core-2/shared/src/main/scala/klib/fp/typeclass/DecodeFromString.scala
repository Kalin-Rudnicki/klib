package klib.fp.typeclass

import java.util.UUID

import scala.util.Try

import cats.syntax.either.*
import io.circe.*
import io.circe.parser.*

import klib.utils.Message

// =====| Core |=====

trait DecodeFromString[+T] {

  def decode(string: String): Either[Message, T]

  final def map[T2](f: T => T2): DecodeFromString[T2] =
    decode(_).map(f)

  final def fMap[T2](f: T => Either[Message, T2]): DecodeFromString[T2] =
    decode(_).flatMap(f)

}
object DecodeFromString {

  def apply[T: DecodeFromString]: DecodeFromString[T] =
    implicitly[DecodeFromString[T]]

  def fromCirceDecoder[T: Decoder]: DecodeFromString[T] =
    decode[T](_).leftMap(Message.fromThrowable(_))

  implicit val stringDecodeString: DecodeFromString[String] =
    _.asRight

  def fromOptionF[R](name: String, f: String => Option[R]): DecodeFromString[R] =
    str => f(str).toRight(Message(s"Malformatted $name '$str'"))

  implicit val booleanDecodeString: DecodeFromString[Boolean] =
    fromOptionF("boolean", _.toBooleanOption)

  implicit val intDecodeString: DecodeFromString[Int] =
    fromOptionF("int", _.toIntOption)

  implicit val longDecodeString: DecodeFromString[Long] =
    fromOptionF("long", _.toLongOption)

  implicit val floatDecodeString: DecodeFromString[Float] =
    fromOptionF("float", _.toFloatOption)

  implicit val doubleDecodeString: DecodeFromString[Double] =
    fromOptionF("double", _.toDoubleOption)

  implicit val uuidDecodeString: DecodeFromString[UUID] =
    str => Try(UUID.fromString(str)).toEither.leftMap(Message.fromThrowable(_))

}

// =====| Extensions |=====
