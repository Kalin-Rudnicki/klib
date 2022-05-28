package klib.web

import cats.data.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import zio.json.*

import klib.fp.typeclass.*
import klib.utils.*

final class ParamMap(mapType: String, map: Map[String, String]) {

  // =====| Add |=====

  private def copy(f: Map[String, String] => Map[String, String]): ParamMap =
    new ParamMap(mapType, f(map))

  def addParam[T: EncodeToString](key: String, value: T): ParamMap =
    copy(_.updated(key, EncodeToString[T].encode(value)))

  def addParamO[T: EncodeToString](key: String, value: Option[T]): ParamMap =
    value match {
      case Some(value) => addParam[T](key, value)
      case None        => this
    }

  def addJsonParam[T: JsonEncoder](key: String, value: T): ParamMap =
    addParam[T](key, value)(using EncodeToString.fromJsonEncoder[T])

  def addJsonParamO[T: JsonEncoder](key: String, value: Option[T]): ParamMap =
    value match {
      case Some(value) => addJsonParam[T](key, value)
      case None        => this
    }

  // =====| Get |=====

  private def genGet[T](key: String)(f: Option[String] => EitherErrorNEL[T]): EitherErrorNEL[T] =
    f(map.get(key))

  private def getRequired[T](key: String, f: String => EitherErrorNEL[T]): EitherErrorNEL[T] =
    genGet[T](key) { mString =>
      for {
        string <- mString.toRightNel(KError.UserError(s"Missing required $mapType '$key'"))
        t <- f(string)
      } yield t
    }

  private def getOption[T](key: String, f: String => EitherErrorNEL[T]): EitherErrorNEL[Option[T]] =
    genGet[Option[T]](key)(_.traverse(f))

  def getParam[T: DecodeFromString](key: String): EitherErrorNEL[T] =
    getRequired(key, implicitly[DecodeFromString[T]].decodeError)

  def getParamO[T: DecodeFromString](key: String): EitherErrorNEL[Option[T]] =
    getOption(key, implicitly[DecodeFromString[T]].decodeError)

  def getJsonParam[T: JsonDecoder](key: String): EitherErrorNEL[T] =
    getParam[T](key)(using DecodeFromString.fromJsonDecoder[T])

  def getJsonParamO[T: JsonDecoder](key: String): EitherErrorNEL[Option[T]] =
    getParamO[T](key)(using DecodeFromString.fromJsonDecoder[T])

}
object ParamMap {

  def empty(mapType: String): ParamMap = new ParamMap(mapType, Map.empty)

}
