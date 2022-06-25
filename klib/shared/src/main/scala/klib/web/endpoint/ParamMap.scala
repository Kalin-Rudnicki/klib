package klib.web.endpoint

import cats.data.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import zio.json.*

import klib.fp.typeclass.*
import klib.utils.*

final class ParamMap(mapType: String, map: Map[String, String]) { self =>

  // =====| Add |=====

  private def copy(f: Map[String, String] => Map[String, String]): ParamMap = ParamMap(mapType, f(map))

  def addParam[T: EncodeToString](key: String, value: T): ParamMap =
    self.copy(_.updated(key, EncodeToString[T].encode(value)))

  def addParamOpt[T: EncodeToString](key: String, value: Option[T]): ParamMap =
    value match {
      case Some(value) => self.addParam[T](key, value)
      case None        => self
    }

  def addJsonParam[T: JsonEncoder](key: String, value: T): ParamMap =
    addParam[T](key, value)(using EncodeToString.fromJsonEncoder[T])

  def addJsonParamOpt[T: JsonEncoder](key: String, value: Option[T]): ParamMap =
    value match {
      case Some(value) => self.addJsonParam[T](key, value)
      case None        => self
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

  // =====| Misc |=====

  override def toString: String =
    map.toList.sortBy(_._1).map { (k, v) => s"$k -> $v" }.mkString(s"ParamMap[$mapType](", ", ", ")")

}
object ParamMap {

  def empty(mapType: String): ParamMap = new ParamMap(mapType, Map.empty)

}
