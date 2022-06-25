package klib.web.endpoint

import cats.syntax.option.*
import monocle.Lens
import monocle.macros.GenLens
import zio.{Unzippable, Zippable}
import zio.json.*

import klib.fp.typeclass.*

// =====| Path Args |=====

final class PathParam[T] private (private[endpoint] val encode: EncodeToString[T], private[endpoint] val decode: DecodeFromString[T])
object PathParam {

  def apply[T: EncodeToString: DecodeFromString]: PathParam[T] =
    new PathParam[T](EncodeToString[T], DecodeFromString[T])

  def json[T: JsonEncoder: JsonDecoder]: PathParam[T] =
    new PathParam[T](EncodeToString.fromJsonEncoder[T], DecodeFromString.fromJsonDecoder[T])

}

object ___

// =====| Query Codec |=====

final class QueryCodec[PathT, ParamMapsT] private (
    method: String,
    pathCodec: QueryCodec.PathCodec[PathT],
    paramMapsCodec: QueryCodec.ParamMapsCodec[ParamMapsT],
) { self =>

  // =====| Encode / Decode |=====

  private[endpoint] def encode(pathT: PathT, paramMapsT: ParamMapsT): (List[String], ParamMap, ParamMap, ParamMap) = {
    val path = pathCodec.encode(pathT)
    val paramMaps = paramMapsCodec.encode(paramMapsT)
    (path, paramMaps.headers, paramMaps.queryParams, paramMaps.cookies)
  }

  private[endpoint] def decode(path: List[String], headers: ParamMap, queryParams: ParamMap, cookies: ParamMap): Option[(PathT, ParamMapsT)] =
    for {
      (pathT, remaining) <- pathCodec.decode(path)
      _ <- Option.when(remaining.isEmpty)(())
      paramMapsT <- paramMapsCodec.decode(ParamMaps(headers, queryParams, cookies))
    } yield (pathT, paramMapsT)

  // =====| Path |=====

  private def mapPathCodec[PathT2](mapF: QueryCodec.PathCodec[PathT] => QueryCodec.PathCodec[PathT2]): QueryCodec[PathT2, ParamMapsT] =
    new QueryCodec[PathT2, ParamMapsT](method, mapF(pathCodec), paramMapsCodec)

  def /:(path: String): QueryCodec[PathT, ParamMapsT] =
    mapPathCodec(_.prependPath(path))

  def /:(paths: List[String]): QueryCodec[PathT, ParamMapsT] =
    mapPathCodec(_.prependPaths(paths))

  def /:[T](pathParam: PathParam[T])(implicit
      unzippable: Unzippable[PathT, T],
      zippable: Zippable[PathT, T],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[zippable.Out, ParamMapsT] =
    mapPathCodec(_.prependPathParam[T](pathParam))

  def /:(@scala.annotation.unused remainingPaths: ___.type)(implicit
      unzippable: Unzippable[PathT, List[String]],
      zippable: Zippable[PathT, List[String]],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[zippable.Out, ParamMapsT] =
    mapPathCodec(_.remainingPaths)

  // =====| Param Maps |=====

  private def mapParamMapsCodec[ParamMapsT2](mapF: QueryCodec.ParamMapsCodec[ParamMapsT] => QueryCodec.ParamMapsCodec[ParamMapsT2]): QueryCodec[PathT, ParamMapsT2] =
    new QueryCodec[PathT, ParamMapsT2](method, pathCodec, mapF(paramMapsCodec))

  // --- headers ---

  def rawHeader[T: EncodeToString: DecodeFromString](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, T],
      zippable: Zippable[ParamMapsT, T],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simple[T](name, GenLens[ParamMaps](_.headers), EncodeToString[T], DecodeFromString[T]))

  def rawHeaderOpt[T: EncodeToString: DecodeFromString](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, Option[T]],
      zippable: Zippable[ParamMapsT, Option[T]],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simpleOpt[T](name, GenLens[ParamMaps](_.headers), EncodeToString[T], DecodeFromString[T]))

  def jsonHeader[T: JsonEncoder: JsonDecoder](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, T],
      zippable: Zippable[ParamMapsT, T],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simple[T](name, GenLens[ParamMaps](_.headers), EncodeToString.fromJsonEncoder[T], DecodeFromString.fromJsonDecoder[T]))

  def jsonHeaderOpt[T: JsonEncoder: JsonDecoder](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, Option[T]],
      zippable: Zippable[ParamMapsT, Option[T]],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simpleOpt[T](name, GenLens[ParamMaps](_.headers), EncodeToString.fromJsonEncoder[T], DecodeFromString.fromJsonDecoder[T]))

  // --- queryParams ---

  def rawQueryParam[T: EncodeToString: DecodeFromString](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, T],
      zippable: Zippable[ParamMapsT, T],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simple[T](name, GenLens[ParamMaps](_.queryParams), EncodeToString[T], DecodeFromString[T]))

  def rawQueryParamOpt[T: EncodeToString: DecodeFromString](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, Option[T]],
      zippable: Zippable[ParamMapsT, Option[T]],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simpleOpt[T](name, GenLens[ParamMaps](_.queryParams), EncodeToString[T], DecodeFromString[T]))

  def jsonQueryParam[T: JsonEncoder: JsonDecoder](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, T],
      zippable: Zippable[ParamMapsT, T],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simple[T](name, GenLens[ParamMaps](_.queryParams), EncodeToString.fromJsonEncoder[T], DecodeFromString.fromJsonDecoder[T]))

  def jsonQueryParamOpt[T: JsonEncoder: JsonDecoder](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, Option[T]],
      zippable: Zippable[ParamMapsT, Option[T]],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simpleOpt[T](name, GenLens[ParamMaps](_.queryParams), EncodeToString.fromJsonEncoder[T], DecodeFromString.fromJsonDecoder[T]))

  // --- cookies ---

  def rawCookie[T: EncodeToString: DecodeFromString](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, T],
      zippable: Zippable[ParamMapsT, T],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simple[T](name, GenLens[ParamMaps](_.cookies), EncodeToString[T], DecodeFromString[T]))

  def rawCookieOpt[T: EncodeToString: DecodeFromString](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, Option[T]],
      zippable: Zippable[ParamMapsT, Option[T]],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simpleOpt[T](name, GenLens[ParamMaps](_.cookies), EncodeToString[T], DecodeFromString[T]))

  def jsonCookie[T: JsonEncoder: JsonDecoder](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, T],
      zippable: Zippable[ParamMapsT, T],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simple[T](name, GenLens[ParamMaps](_.cookies), EncodeToString.fromJsonEncoder[T], DecodeFromString.fromJsonDecoder[T]))

  def jsonCookieOpt[T: JsonEncoder: JsonDecoder](name: String)(implicit
      unzippable: Unzippable[ParamMapsT, Option[T]],
      zippable: Zippable[ParamMapsT, Option[T]],
      zipsAreSame: zippable.Out =:= unzippable.In,
  ): QueryCodec[PathT, zippable.Out] =
    mapParamMapsCodec(_.simpleOpt[T](name, GenLens[ParamMaps](_.cookies), EncodeToString.fromJsonEncoder[T], DecodeFromString.fromJsonDecoder[T]))

}
object QueryCodec {

  def apply(method: String): QueryCodec[Unit, Unit] =
    new QueryCodec[Unit, Unit](method, PathCodec.Empty, ParamMapsCodec.Empty)

  val get: QueryCodec[Unit, Unit] = QueryCodec("GET")
  val post: QueryCodec[Unit, Unit] = QueryCodec("POST")
  val patch: QueryCodec[Unit, Unit] = QueryCodec("PATCH")
  val delete: QueryCodec[Unit, Unit] = QueryCodec("DELETE")

  // =====| Path Codec |=====

  private final class PathCodec[T](
      val encode: T => List[String],
  )(
      val decode: List[String] => Option[(T, List[String])],
  ) { self =>

    def prependPath(path: String): PathCodec[T] =
      PathCodec[T](t => path :: encode(t)) {
        case head :: tail =>
          if (head == path) decode(tail)
          else None
        case Nil =>
          None
      }

    def prependPaths(paths: List[String]): PathCodec[T] =
      paths.foldRight(self) { (path, pathCodec) => pathCodec.prependPath(path) }

    def prependPathParam[T2](pathParam: PathParam[T2])(implicit
        unzippable: Unzippable[T, T2],
        zippable: Zippable[T, T2],
        zipsAreSame: zippable.Out =:= unzippable.In,
    ): PathCodec[zippable.Out] =
      PathCodec[zippable.Out] { t =>
        val (t1, t2) = unzippable.unzip(t)
        pathParam.encode.encode(t2) :: encode(t1)
      } {
        case head :: tail =>
          for {
            t2 <- pathParam.decode.decode(head).toOption
            (t1, remaining) <- decode(tail)
          } yield (zippable.zip(t1, t2), remaining)
        case Nil =>
          None
      }

    def remainingPaths(implicit
        unzippable: Unzippable[T, List[String]],
        zippable: Zippable[T, List[String]],
        zipsAreSame: zippable.Out =:= unzippable.In,
    ): PathCodec[zippable.Out] =
      PathCodec[zippable.Out] { t =>
        val (t1, t2) = unzippable.unzip(t)
        t2 ::: encode(t1)
      } { paths =>
        for {
          (t1, remaining) <- decode(paths)
        } yield (zippable.zip(t1, remaining), Nil)
      }

  }
  private object PathCodec {

    val Empty: PathCodec[Unit] =
      PathCodec[Unit] { _ => Nil } { paths => ((), paths).some }

  }

  // =====| Param Maps Codec |=====

  private final class ParamMapsCodec[T](
      val encode: T => ParamMaps,
  )(
      val decode: ParamMaps => Option[T],
  ) { self =>

    def simple[T2](
        name: String,
        lens: Lens[ParamMaps, ParamMap],
        encodeToString: EncodeToString[T2],
        decodeFromString: DecodeFromString[T2],
    )(implicit
        unzippable: Unzippable[T, T2],
        zippable: Zippable[T, T2],
        zipsAreSame: zippable.Out =:= unzippable.In,
    ): ParamMapsCodec[zippable.Out] =
      ParamMapsCodec[zippable.Out] { t =>
        val (t1, t2) = unzippable.unzip(t)
        lens.modify(_.addParam[T2](name, t2)(using encodeToString))(encode(t1))
      } { paramMaps =>
        for {
          t1 <- decode(paramMaps)
          t2 <- lens.get(paramMaps).getParam[T2](name)(using decodeFromString).toOption
        } yield zippable.zip(t1, t2)
      }

    def simpleOpt[T2](
        name: String,
        lens: Lens[ParamMaps, ParamMap],
        encodeToString: EncodeToString[T2],
        decodeFromString: DecodeFromString[T2],
    )(implicit
        unzippable: Unzippable[T, Option[T2]],
        zippable: Zippable[T, Option[T2]],
        zipsAreSame: zippable.Out =:= unzippable.In,
    ): ParamMapsCodec[zippable.Out] =
      ParamMapsCodec[zippable.Out] { t =>
        val (t1, t2) = unzippable.unzip(t)
        lens.modify(_.addParamOpt[T2](name, t2)(using encodeToString))(encode(t1))
      } { paramMaps =>
        for {
          t1 <- decode(paramMaps)
          t2 <- lens.get(paramMaps).getParamO[T2](name)(using decodeFromString).toOption
        } yield zippable.zip(t1, t2)
      }

  }
  private object ParamMapsCodec {

    val Empty: ParamMapsCodec[Unit] =
      ParamMapsCodec[Unit] { _ => ParamMaps.Empty } { _ => ().some }

  }

}
