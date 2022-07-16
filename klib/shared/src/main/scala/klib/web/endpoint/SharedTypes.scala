package klib.web.endpoint

import klib.utils.*

object SharedTypes {

  type QueryCodecT[ET <: EndpointType[_, _, _, _]] =
    ET match { case EndpointType[pathT, paramMapsT, _, _] => QueryCodec[pathT, paramMapsT] }

  type BodyT[FileT, InputB <: Body] =
    InputB match {
      case Body.None       => Unit
      case Body.Raw        => String
      case Body.Encoded[o] => o
      case Body.File       => FileT
    }

  type Impl[R, FileIn, FileOut] =
    [ET <: EndpointType[_, _, _, _]] =>> ET match {
      case EndpointType[pathT, paramMapT, inputB, outputB] =>
        (pathT, paramMapT, BodyT[FileIn, inputB]) => KRIO[R, BodyT[FileOut, outputB]]
    }

}
