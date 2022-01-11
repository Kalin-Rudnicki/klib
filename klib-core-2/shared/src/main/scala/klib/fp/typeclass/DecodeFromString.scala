package klib.fp.typeclass

import java.util.UUID

import scala.util.Try

import io.circe._
import io.circe.parser._
import zio._

// =====| Core |=====

trait DecodeFromString[+T] {

  def decode(string: String): Task[T]

  final def map[T2](f: T => T2): DecodeFromString[T2] =
    decode(_).map(f)

  final def fMap[T2](f: T => Task[T2]): DecodeFromString[T2] =
    decode(_).flatMap(f)

}
object DecodeFromString {

  def apply[T: DecodeFromString]: DecodeFromString[T] =
    implicitly[DecodeFromString[T]]

  def fromCirceDecoder[T: Decoder]: DecodeFromString[T] =
    str => ZIO.fromEither(decode(str))

  implicit val stringDecodeString: DecodeFromString[String] =
    ZIO.succeed(_)

  def fromOptionF[R](name: String, f: String => Option[R]): DecodeFromString[R] =
    str => ZIO.fromOption(f(str)).mapError(_ => new RuntimeException(s"Malformatted $name '$str'"))

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
    str => ZIO.attempt(UUID.fromString(str))

}

// =====| Extensions |=====
