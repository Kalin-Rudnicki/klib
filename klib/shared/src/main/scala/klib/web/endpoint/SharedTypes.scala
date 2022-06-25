package klib.web.endpoint

object SharedTypes {

  // TODO (KR) : Remove?
  type InputsFT[PathT, ParamMapT, Res] =
    (PathT, ParamMapT) match {
      case (Unit, Unit)       => Res
      case (Unit, paramMapT)  => paramMapT => Res
      case (pathT, Unit)      => pathT => Res
      case (pathT, paramMapT) => (pathT, paramMapT) => Res
    }

  type QueryCodecT[ET <: EndpointType[_, _, _, _]] =
    ET match { case EndpointType[pathT, paramMapsT, _, _] => QueryCodec[pathT, paramMapsT] }

}
