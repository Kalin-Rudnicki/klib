package klib.fp.typeclass

import cats.data.*
import cats.syntax.either.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import java.time.*
import java.util.UUID
import scala.util.Try
import zio.json.*

import klib.utils.*

// =====| Core |=====

trait DecodeFromString[+T] {

  def decode(string: String): EitherNel[String, T]

  final def decodeError(string: String): EitherErrorNEL[T] =
    decode(string).leftMap {
      _.map(msg => KError.UserError(s"Unable to decode JSON : $msg"))
    }

  final def map[T2](f: T => T2): DecodeFromString[T2] =
    decode(_).map(f)

  final def fMap[T2](f: T => EitherNel[String, T2]): DecodeFromString[T2] =
    decode(_).flatMap(f)

  final def commaSeparatedList: DecodeFromString[List[T]] = { str =>
    if (str.isEmpty) Nil.asRight
    else str.split(",").toList.traverse(decode)
  }

}
object DecodeFromString {

  def apply[T: DecodeFromString]: DecodeFromString[T] =
    implicitly[DecodeFromString[T]]

  def fromJsonDecoder[T: JsonDecoder]: DecodeFromString[T] =
    JsonDecoder[T].decodeJson(_).leftMap(NonEmptyList.one)

  implicit val stringDecodeString: DecodeFromString[String] =
    _.asRight

  def fromOptionF[R](name: String, f: String => Option[R]): DecodeFromString[R] =
    str => f(str).toRight(NonEmptyList.one(s"Malformatted $name '$str'"))

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
    str => Try(UUID.fromString(str)).toEither.leftMap(e => NonEmptyList.one(e.getMessage))

  private object temporal {
    val numsOneTwo = "(\\d{1,2})"
    val numsTwo = "(\\d{2})"
    val numsFour = "(\\d{4})"
    val someSpacing = "\\s+".r
  }

  def configurableLocalDateDecodeString(_currentYear: => Int, futureTolerance: => Int): DecodeFromString[LocalDate] = { str =>
    import temporal.*

    val us2Year = s"$numsOneTwo/$numsOneTwo/$numsTwo".r
    val us4Year = s"$numsOneTwo/$numsOneTwo/$numsFour".r
    val other2Year = s"$numsOneTwo-$numsOneTwo-$numsTwo".r
    val other4Year = s"$numsOneTwo-$numsOneTwo-$numsFour".r
    val yearFirst = s"$numsFour-$numsOneTwo-$numsOneTwo".r

    def guessYear(y: Int): Int = {
      val currentYear = _currentYear
      val currentCentury = currentYear / 100
      val currentYearInCentury = currentYear % 100
      val futureYearInCentury = currentYearInCentury + futureTolerance
      val centuryGuess =
        if (futureYearInCentury >= 100)
          if (y >= currentYearInCentury) currentCentury
          else if (y <= futureYearInCentury % 100) currentCentury + 1
          else currentCentury
        else if (y <= futureYearInCentury) currentCentury
        else currentCentury - 1

      centuryGuess * 100 + y
    }

    def attemptDate(date: => LocalDate): EitherNel[String, LocalDate] =
      Try(date).toEither.leftMap(_.getMessage).toEitherNel

    str match {
      case us2Year(month, day, year)    => attemptDate(LocalDate.of(guessYear(year.toInt), month.toInt, day.toInt))
      case us4Year(month, day, year)    => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case other2Year(day, month, year) => attemptDate(LocalDate.of(guessYear(year.toInt), month.toInt, day.toInt))
      case other4Year(day, month, year) => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case yearFirst(year, month, day)  => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case _                            => s"Malformatted date ${str.unesc}".leftNel
    }
  }

  implicit val localDateDecodeString: DecodeFromString[LocalDate] = configurableLocalDateDecodeString(LocalDate.now.getYear, 10)

  implicit val localTimeDecodeString: DecodeFromString[LocalTime] = { str =>
    import temporal.*

    val hourMinute = s"$numsOneTwo:$numsTwo".r
    val hourMinuteAM = s"$numsOneTwo:$numsTwo$someSpacing(?:AM|am)".r
    val hourMinutePM = s"$numsOneTwo:$numsTwo$someSpacing(?:PM|pm)".r
    val hourMinuteSecond = s"$numsOneTwo:$numsTwo$numsTwo".r
    val hourMinuteSecondAM = s"$numsOneTwo:$numsTwo$numsTwo$someSpacing(?:AM|am)".r
    val hourMinuteSecondPM = s"$numsOneTwo:$numsTwo$numsTwo$someSpacing(?:PM|pm)".r

    def attemptTime(time: => LocalTime): EitherNel[String, LocalTime] =
      Try(time).toEither.leftMap(_.getMessage).toEitherNel

    str match {
      case hourMinute(hour, minute)                 => attemptTime(LocalTime.of(hour.toInt, minute.toInt))
      case hourMinuteAM(hour, minute)               => attemptTime(LocalTime.of(hour.toInt, minute.toInt))
      case hourMinutePM(hour, minute)               => attemptTime(LocalTime.of(hour.toInt + 12, minute.toInt))
      case hourMinuteSecond(hour, minute, second)   => attemptTime(LocalTime.of(hour.toInt, minute.toInt, second.toInt))
      case hourMinuteSecondAM(hour, minute, second) => attemptTime(LocalTime.of(hour.toInt, minute.toInt, second.toInt))
      case hourMinuteSecondPM(hour, minute, second) => attemptTime(LocalTime.of(hour.toInt + 12, minute.toInt, second.toInt))
      case _                                        => s"Malformatted time ${str.unesc}".leftNel
    }
  }

  def configurableLocalDateTimeDecodeString(_currentYear: => Int, futureTolerance: => Int): DecodeFromString[LocalDateTime] = { str =>
    val splitStr = "\\s+(?!(\\s|AM|PM|am|pm))"

    str.split(splitStr) match {
      case Array(str1, str2) =>
        (configurableLocalDateDecodeString(_currentYear, futureTolerance).decode(str1), localTimeDecodeString.decode(str2)).parTupled match {
          case Right((date, time)) => LocalDateTime.of(date, time).asRight
          case Left(e1) =>
            (localTimeDecodeString.decode(str1), configurableLocalDateDecodeString(_currentYear, futureTolerance).decode(str2)).parTupled match {
              case Right((time, date)) => LocalDateTime.of(date, time).asRight
              case Left(e2) =>
                NonEmptyList
                  .of(
                    s"Could not parse 'DATE TIME' : ${e1.toList.mkString(", ")}",
                    s"Could not parse 'TIME DATE' : ${e2.toList.mkString(", ")}",
                  )
                  .asLeft
            }
        }
      case Array(str1) => localDateDecodeString.decode(str1).map(_.atStartOfDay)
      case _           => s"Malformatted date-time ${str.unesc}".leftNel
    }
  }

  implicit val localDateTimeDecodeString: DecodeFromString[LocalDateTime] = configurableLocalDateTimeDecodeString(LocalDate.now.getYear, 10)

}

// =====| Extensions |=====
