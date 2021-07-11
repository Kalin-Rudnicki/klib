package klib.fp.typeclass

import klib.extensions.{given, _}
import klib.fp.types._

trait DecodeString[+T] {

  def attemptDecode(string: String): ??[T]

}

object DecodeString {

  object extensions {

    extension (string: String)
      def attemptDecode[T: DecodeString]: ??[T] =
        summon[DecodeString[T]].attemptDecode(string)

  }

  object instances {

    given stringDecodeString: DecodeString[String] with {
      def attemptDecode(string: String): ??[String] = {
        import ErrorAccumulator.instances.given
        string.pure[??]
      }
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

    given decodeStringList[R: DecodeString]: DecodeString[List[R]] = { s =>
      import ErrorAccumulator.instances.given
      s.split(",").toList.map(_.attemptDecode[R]).traverse
    }

  }

}
