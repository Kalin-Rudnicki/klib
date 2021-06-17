package klib.fp.typeclass

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

  private def makeDecoder[R](name: String, f: String => Option[R]): DecodeString[R] =
    s => f(s).toMaybe.toEA(Message(s"Malformatted $name '$s'"))

  "".toBoolean

  implicit val booleanDecodeString: DecodeString[Boolean] =
    makeDecoder(
      "boolean",
      s =>
        if (s.equalsIgnoreCase("true"))
          true.someOpt
        else if (s.equalsIgnoreCase("false"))
          false.someOpt
        else
          scala.None,
    )

  implicit val intDecodeString: DecodeString[Int] =
    makeDecoder("int", _.toIntOption)

  implicit val longDecodeString: DecodeString[Long] =
    makeDecoder("long", _.toLongOption)

  implicit val floatDecodeString: DecodeString[Float] =
    makeDecoder("float", _.toFloatOption)

  implicit val doubleDecodeString: DecodeString[Double] =
    makeDecoder("double", _.toDoubleOption)

  implicit def decodeStringList[R: DecodeString]: DecodeString[List[R]] =
    s => s.split(",").toList.map(implicitly[DecodeString[R]].decode).traverse

}
