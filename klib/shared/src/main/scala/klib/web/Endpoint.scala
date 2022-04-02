package klib.web

import cats.data.*
import cats.syntax.either.*
import cats.syntax.parallel.*
import zio.{Unzippable, Zippable}
import zio.json.*

import klib.fp.typeclass.*
import klib.utils.*

final class Endpoint[
    Headers,
    QueryParams,
    RequestBody <: Endpoint.Types.RequestBody[_],
    ResponseBody <: Endpoint.Types.ResponseBody[_],
] private (
    private[web] val path: Endpoint.Types.Path,
    private[web] val header: Endpoint.Types.MapCodec[Headers],
    private[web] val query: Endpoint.Types.MapCodec[QueryParams],
    private[web] val request: RequestBody,
    private[web] val response: ResponseBody,
)
object Endpoint {

  // =====| Type Aliases |=====

  // --- No Response ---

  type None_None[Headers, QueryParams] =
    Endpoint[Headers, QueryParams, Types.RequestBody.None.type, Types.ResponseBody.None.type]

  type Encoded_None[Headers, QueryParams, RequestBody] =
    Endpoint[Headers, QueryParams, Types.RequestBody.Encoded[RequestBody], Types.ResponseBody.None.type]

  type File_None[Headers, QueryParams] =
    Endpoint[Headers, QueryParams, Types.RequestBody.File.type, Types.ResponseBody.None.type]

  // --- Encoded Response ---

  type None_Encoded[Headers, QueryParams, ResponseBody] =
    Endpoint[Headers, QueryParams, Types.RequestBody.None.type, Types.ResponseBody.Encoded[ResponseBody]]

  type Encoded_Encoded[Headers, QueryParams, RequestBody, ResponseBody] =
    Endpoint[Headers, QueryParams, Types.RequestBody.Encoded[RequestBody], Types.ResponseBody.Encoded[ResponseBody]]

  type File_Encoded[Headers, QueryParams, ResponseBody] =
    Endpoint[Headers, QueryParams, Types.RequestBody.File.type, Types.ResponseBody.Encoded[ResponseBody]]

  // --- File Response ---

  type None_File[Headers, QueryParams] =
    Endpoint[Headers, QueryParams, Types.RequestBody.None.type, Types.ResponseBody.File.type]

  type Encoded_File[Headers, QueryParams, RequestBody] =
    Endpoint[Headers, QueryParams, Types.RequestBody.Encoded[RequestBody], Types.ResponseBody.File.type]

  type File_File[Headers, QueryParams] =
    Endpoint[Headers, QueryParams, Types.RequestBody.File.type, Types.ResponseBody.File.type]

  // =====| Types |=====

  object Types {

    final case class Path(
        paths: List[String],
    ) {

      def prepended(paths: List[String]): Path =
        Path(paths ::: this.paths)

      def appended(paths: List[String]): Path =
        Path(this.paths ::: paths)

      def replaced(paths: List[String]): Path =
        Path(paths)

    }
    object Path {

      val empty: Path = Path(Nil)

    }

    sealed trait RequestBody[+T]
    object RequestBody {
      case object None extends RequestBody[Nothing]
      case object File extends RequestBody[Nothing]
      final case class Encoded[T](encoder: EncodeToString[T], decoder: DecodeFromString[T]) extends RequestBody[T]
    }

    sealed trait ResponseBody[+T]
    object ResponseBody {
      case object None extends ResponseBody[Nothing]
      case object File extends ResponseBody[Nothing]
      final case class Encoded[T](encoder: EncodeToString[T], decoder: DecodeFromString[T]) extends ResponseBody[T]
    }

    final case class MapCodec[T](
        mapType: String,
        requiredKeys: Set[String],
        optionalKeys: Set[String],
        encode: (ParamMap, T) => ParamMap,
        decode: ParamMap => EitherError[T],
    ) {

      def zip[T2](
          required: Boolean,
          key: String,
          get: ParamMap => EitherError[T2],
          set: (ParamMap, T2) => ParamMap,
      )(implicit
          unzippable: Unzippable[T, T2],
          zippable: Zippable[T, T2],
          zipsAreSame: unzippable.In =:= zippable.Out,
      ): MapCodec[zippable.Out] =
        MapCodec[zippable.Out](
          mapType = mapType,
          requiredKeys = if (required) requiredKeys + key else requiredKeys,
          optionalKeys = if (required) optionalKeys else optionalKeys + key,
          encode = { (pm, t) =>
            val (t1, t2) = unzippable.unzip(zipsAreSame.flip(t))
            set(encode(pm, t1), t2)
          },
          decode = { pm =>
            (
              decode(pm),
              get(pm),
            ).parMapN(zippable.zip)
          },
        )

    }
    object MapCodec {

      def empty(mapType: String): MapCodec[Unit] =
        MapCodec[Unit](
          mapType = mapType,
          requiredKeys = Set.empty,
          optionalKeys = Set.empty,
          encode = (pm, _) => pm,
          decode = _ => ().asRight,
        )

    }

  }

  def builder: Builder[
    Unit,
    Unit,
    Types.RequestBody.None.type,
    Types.ResponseBody.None.type,
  ] =
    new Builder[
      Unit,
      Unit,
      Types.RequestBody.None.type,
      Types.ResponseBody.None.type,
    ](
      _path = Types.Path.empty,
      _header = Types.MapCodec.empty("header"),
      _queryParams = Types.MapCodec.empty("param"),
      _request = Types.RequestBody.None,
      _response = Types.ResponseBody.None,
    )

  final case class Builder[
      Headers,
      QueryParams,
      RequestBody <: Types.RequestBody[_],
      ResponseBody <: Types.ResponseBody[_],
  ] private[Endpoint] (
      private[Endpoint] val _path: Types.Path,
      private[Endpoint] val _header: Types.MapCodec[Headers],
      private[Endpoint] val _queryParams: Types.MapCodec[QueryParams],
      private[Endpoint] val _request: RequestBody,
      private[Endpoint] val _response: ResponseBody,
  ) {

    // --- Path ---

    private def copyPath(f: Types.Path => Types.Path): Builder[Headers, QueryParams, RequestBody, ResponseBody] =
      copy(_path = f(_path))

    def prependPaths(paths: String*): Builder[Headers, QueryParams, RequestBody, ResponseBody] =
      copyPath(_.prepended(paths.toList))

    def appendPaths(paths: String*): Builder[Headers, QueryParams, RequestBody, ResponseBody] =
      copyPath(_.appended(paths.toList))

    def replacePaths(paths: String*): Builder[Headers, QueryParams, RequestBody, ResponseBody] =
      copyPath(_.replaced(paths.toList))

    // --- Request Body ---

    private def copyRequestBody[T <: Types.RequestBody[_]](request: T): Builder[Headers, QueryParams, T, ResponseBody] =
      copy(_request = request)

    def noRequestBody: Builder[Headers, QueryParams, Types.RequestBody.None.type, ResponseBody] =
      copyRequestBody(Types.RequestBody.None)

    def requestBody[T: EncodeToString: DecodeFromString]: Builder[
      Headers,
      QueryParams,
      Types.RequestBody.Encoded[T],
      ResponseBody,
    ] =
      copyRequestBody(
        Types.RequestBody.Encoded[T](
          EncodeToString[T],
          DecodeFromString[T],
        ),
      )

    def jsonRequestBody[T: JsonCodec]: Builder[
      Headers,
      QueryParams,
      Types.RequestBody.Encoded[T],
      ResponseBody,
    ] =
      copyRequestBody(
        Types.RequestBody.Encoded[T](
          EncodeToString.fromJsonEncoder[T],
          DecodeFromString.fromJsonDecoder[T],
        ),
      )

    def fileRequestBody: Builder[Headers, QueryParams, Types.RequestBody.File.type, ResponseBody] =
      copyRequestBody(Types.RequestBody.File)

    // --- Response Body ---

    private def copyResponseBody[T <: Types.ResponseBody[_]](response: T): Builder[Headers, QueryParams, RequestBody, T] =
      copy(_response = response)

    def noResponseBody: Builder[Headers, QueryParams, RequestBody, Types.ResponseBody.None.type] =
      copyResponseBody(Types.ResponseBody.None)

    def responseBody[T: EncodeToString: DecodeFromString]: Builder[
      Headers,
      QueryParams,
      RequestBody,
      Types.ResponseBody.Encoded[T],
    ] =
      copyResponseBody(
        Types.ResponseBody.Encoded[T](
          EncodeToString[T],
          DecodeFromString[T],
        ),
      )

    def jsonResponseBody[T: JsonEncoder: JsonDecoder]: Builder[
      Headers,
      QueryParams,
      RequestBody,
      Types.ResponseBody.Encoded[T],
    ] =
      copyResponseBody(
        Types.ResponseBody.Encoded[T](
          EncodeToString.fromJsonEncoder[T],
          DecodeFromString.fromJsonDecoder[T],
        ),
      )

    def fileResponseBody: Builder[Headers, QueryParams, RequestBody, Types.ResponseBody.File.type] =
      copyResponseBody(Types.ResponseBody.File)

    // --- Headers ---

    private def zipHeader[H](
        required: Boolean,
        key: String,
        get: ParamMap => EitherError[H],
        set: (ParamMap, H) => ParamMap,
    )(implicit
        unzippable: Unzippable[Headers, H],
        zippable: Zippable[Headers, H],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[zippable.Out, QueryParams, RequestBody, ResponseBody] =
      copy(_header = _header.zip(required, key, get, set))

    def header[H: EncodeToString: DecodeFromString](key: String)(implicit
        unzippable: Unzippable[Headers, H],
        zippable: Zippable[Headers, H],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[zippable.Out, QueryParams, RequestBody, ResponseBody] =
      zipHeader[H](
        true,
        key,
        _.getParam[H](key),
        (pm, h) => pm.addParam[H](key, h),
      )

    def headerO[H: EncodeToString: DecodeFromString](key: String)(implicit
        unzippable: Unzippable[Headers, Option[H]],
        zippable: Zippable[Headers, Option[H]],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[zippable.Out, QueryParams, RequestBody, ResponseBody] =
      zipHeader[Option[H]](
        false,
        key,
        _.getParamO[H](key),
        (pm, h) => pm.addParamO[H](key, h),
      )

    def jsonHeader[H: JsonEncoder: JsonDecoder](key: String)(implicit
        unzippable: Unzippable[Headers, H],
        zippable: Zippable[Headers, H],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[zippable.Out, QueryParams, RequestBody, ResponseBody] =
      zipHeader[H](
        true,
        key,
        _.getJsonParam[H](key),
        (pm, h) => pm.addJsonParam[H](key, h),
      )

    def jsonHeaderO[H: JsonEncoder: JsonDecoder](key: String)(implicit
        unzippable: Unzippable[Headers, Option[H]],
        zippable: Zippable[Headers, Option[H]],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[zippable.Out, QueryParams, RequestBody, ResponseBody] =
      zipHeader[Option[H]](
        false,
        key,
        _.getJsonParamO[H](key),
        (pm, h) => pm.addJsonParamO[H](key, h),
      )

    // --- Params ---

    private def zipParam[P](
        required: Boolean,
        key: String,
        get: ParamMap => EitherError[P],
        set: (ParamMap, P) => ParamMap,
    )(implicit
        unzippable: Unzippable[QueryParams, P],
        zippable: Zippable[QueryParams, P],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[Headers, zippable.Out, RequestBody, ResponseBody] =
      copy(_queryParams = _queryParams.zip(required, key, get, set))

    def param[P: EncodeToString: DecodeFromString](key: String)(implicit
        unzippable: Unzippable[QueryParams, P],
        zippable: Zippable[QueryParams, P],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[Headers, zippable.Out, RequestBody, ResponseBody] =
      zipParam[P](
        true,
        key,
        _.getParam[P](key),
        (pm, h) => pm.addParam[P](key, h),
      )

    def paramO[P: EncodeToString: DecodeFromString](key: String)(implicit
        unzippable: Unzippable[QueryParams, Option[P]],
        zippable: Zippable[QueryParams, Option[P]],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[Headers, zippable.Out, RequestBody, ResponseBody] =
      zipParam[Option[P]](
        false,
        key,
        _.getParamO[P](key),
        (pm, h) => pm.addParamO[P](key, h),
      )

    def jsonParam[P: JsonEncoder: JsonDecoder](key: String)(implicit
        unzippable: Unzippable[QueryParams, P],
        zippable: Zippable[QueryParams, P],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[Headers, zippable.Out, RequestBody, ResponseBody] =
      zipParam[P](
        true,
        key,
        _.getJsonParam[P](key),
        (pm, h) => pm.addJsonParam[P](key, h),
      )

    def jsonParamO[P: JsonEncoder: JsonDecoder](key: String)(implicit
        unzippable: Unzippable[QueryParams, Option[P]],
        zippable: Zippable[QueryParams, Option[P]],
        zipsAreSame: unzippable.In =:= zippable.Out,
    ): Builder[Headers, zippable.Out, RequestBody, ResponseBody] =
      zipParam[Option[P]](
        false,
        key,
        _.getJsonParamO[P](key),
        (pm, h) => pm.addJsonParamO[P](key, h),
      )

    // --- Build ---

    def build: Endpoint[Headers, QueryParams, RequestBody, ResponseBody] =
      Endpoint[Headers, QueryParams, RequestBody, ResponseBody](
        _path,
        _header,
        _queryParams,
        _request,
        _response,
      )

  }

}
