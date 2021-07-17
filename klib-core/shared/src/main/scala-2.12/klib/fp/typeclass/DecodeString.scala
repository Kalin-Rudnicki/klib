package klib.fp.typeclass

import scala.util.Try

import klib.Implicits._
import klib.fp.types._

trait DecodeString[+T] {
  def decode(string: String): ?[T]
}

object DecodeString {

  trait Implicits {

    implicit class DSStringOps(s: String) {

      def attemptDecode[T: DecodeString]: ?[T] =
        implicitly[DecodeString[T]].decode(s)

    }

  }
  object Implicits extends Implicits

  implicit val stringDecodeString: DecodeString[String] =
    _.pure[?]

  private def makeDecoder[R](name: String, f: String => R): DecodeString[R] =
    s => Try(f(s)).toOption.toMaybe.toEA(Message(s"Malformatted $name '$s'"))

  implicit val booleanDecodeString: DecodeString[Boolean] =
    makeDecoder("boolean", _.toBoolean)

  implicit val intDecodeString: DecodeString[Int] =
    makeDecoder("int", _.toInt)

  implicit val longDecodeString: DecodeString[Long] =
    makeDecoder("long", _.toLong)

  implicit val floatDecodeString: DecodeString[Float] =
    makeDecoder("float", _.toFloat)

  implicit val doubleDecodeString: DecodeString[Double] =
    makeDecoder("double", _.toDouble)

  implicit def decodeStringList[R: DecodeString]: DecodeString[List[R]] =
    s => s.split(",").toList.map(implicitly[DecodeString[R]].decode).traverse

}
