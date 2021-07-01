package klib.fp.typeclass

import klib.extensions._
import klib.fp.types.ErrorAccumulator.instances.errorAccumulatorMonad
import klib.fp.types._

trait DecodeString[+T] {

  extension (string: String) def attemptDecode: ??[T]

}

object DecodeString {

  given stringDecodeString: DecodeString[String] with {
    extension (string: String) def attemptDecode: ??[String] = string.pure[??]
  }

  private def makeDecoder[R](name: String, f: String => Option[R]): DecodeString[R] =
    s => f(s).toMaybe.toEA(Message(s"Malformatted $name '$s'"))

  given booleanDecodeString: DecodeString[Boolean] =
    makeDecoder("boolean", _.toBooleanOption)

  given intDecodeString: DecodeString[Int] =
    makeDecoder("int", _.toIntOption)

  given longDecodeString: DecodeString[Long] =
    makeDecoder("long", _.toLongOption)

  given floatDecodeString: DecodeString[Float] =
    makeDecoder("float", _.toFloatOption)

  given doubleDecodeString: DecodeString[Double] =
    makeDecoder("double", _.toDoubleOption)

  given decodeStringList[R: DecodeString]: DecodeString[List[R]] =
    s => s.split(",").toList.map(_.attemptDecode[R]).traverse

}
