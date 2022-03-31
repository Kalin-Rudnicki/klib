package klib.web

import cats.data.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.*

import klib.fp.typeclass.*
import klib.utils.*

final class ParamMap(mapType: String, map: Map[String, String]) {

  // =====| Add |=====

  private def copy(f: Map[String, String] => Map[String, String]): ParamMap =
    new ParamMap(mapType, f(map))

  def addParam[T: EncodeToString](key: String, value: T): ParamMap =
    copy(_.updated(key, implicitly[EncodeToString[T]].encode(value)))

  def addParamO[T: EncodeToString](key: String, value: Option[T]): ParamMap =
    value match {
      case Some(value) => addParam[T](key, value)
      case None        => this
    }

  def addJsonParam[T: Encoder](key: String, value: T): ParamMap =
    copy(_.updated(key, Encoder[T].apply(value).noSpaces))

  def addJsonParamO[T: Encoder](key: String, value: Option[T]): ParamMap =
    value match {
      case Some(value) => addJsonParam[T](key, value)
      case None        => this
    }

  // =====| Get |=====

  private def genGet[T](key: String)(f: Option[String] => EitherError[T]): EitherError[T] =
    f(map.get(key))

  private def getRequired[T](key: String, f: String => EitherError[T]): EitherError[T] =
    genGet[T](key) { mString =>
      for {
        string <- mString.toRight(KError.message.same(s"Missing required $mapType '$key'"))
        t <- f(string)
      } yield t
    }

  private def getOption[T](key: String, f: String => EitherError[T]): EitherError[Option[T]] =
    genGet[Option[T]](key)(_.traverse(f))

  def getParam[T: DecodeFromString](key: String): EitherError[T] =
    getRequired(key, implicitly[DecodeFromString[T]].decodeError)

  def getParamO[T: DecodeFromString](key: String): EitherError[Option[T]] =
    getOption(key, implicitly[DecodeFromString[T]].decodeError)

  def getJsonParam[T: Decoder](key: String): EitherError[T] =
    getRequired(key, DecodeFromString.fromCirceDecoder[T].decodeError)

  def getJsonParamO[T: Decoder](key: String): EitherError[Option[T]] =
    getOption(key, DecodeFromString.fromCirceDecoder[T].decodeError)

}
object ParamMap {

  def empty(mapType: String): ParamMap = new ParamMap(mapType, Map.empty)

}
