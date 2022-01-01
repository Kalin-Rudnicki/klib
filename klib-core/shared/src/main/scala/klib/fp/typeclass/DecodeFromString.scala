package klib.fp.typeclass

import java.util.UUID

import scala.util.Try

import io.circe._
import io.circe.parser._

import klib.Implicits._
import klib.fp.types._

trait DecodeFromString[+T] {

  def decode(string: String): ?[T]

  final def map[T2](f: T => T2): DecodeFromString[T2] =
    decode(_).map(f)

  final def fMap[T2](f: T => ?[T2]): DecodeFromString[T2] =
    decode(_).flatMap(f)

}

object DecodeFromString {

  trait Implicits {

    implicit class DSStringOps(s: String) {

      def attemptDecode[T: DecodeFromString]: ?[T] =
        implicitly[DecodeFromString[T]].decode(s)

    }

  }
  object Implicits extends Implicits

  // =====|  |=====

  def apply[T: DecodeFromString]: DecodeFromString[T] =
    implicitly[DecodeFromString[T]]

  def fromCirceDecoder[T: Decoder]: DecodeFromString[T] =
    decode[T](_).toErrorAccumulator

  implicit val stringDecodeString: DecodeFromString[String] =
    _.pure[?]

  def fromOptionF[R](name: String, f: String => Option[R]): DecodeFromString[R] =
    s => f(s).toMaybe.toEA(Message(s"Malformatted $name '$s'"))

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

  implicit def decodeStringList[R: DecodeFromString]: DecodeFromString[List[R]] =
    s => s.split(",").toList.map(implicitly[DecodeFromString[R]].decode).traverse

  implicit val uuidDecodeString: DecodeFromString[UUID] =
    str => Try { UUID.fromString(str) }.to_?

}
