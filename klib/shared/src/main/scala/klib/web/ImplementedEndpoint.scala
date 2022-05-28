package klib.web

import cats.data.*
import scala.annotation.targetName
import zio.*

import klib.utils.*

final case class ImplementedEndpoint[-R] private (
    pathMatch: List[String] => Boolean,
    handleRequest: HttpRequest => KRIO[R, HttpResponse],
)
object ImplementedEndpoint {

  // TODO (KR) : Note for when cookies need to be handled
  //           : Add it to the [R] type
  //           : Cookies.set("key", "value") ~ def Cookies.set(key: String, value: String): URIO[Cookies, Unit]
  //           : Cookies.delete("key") ~ def Cookies.delete(key: String): URIO[Cookies, Unit]

  def apply[H, P, R](endpoint: Endpoint[H, P, _, _])(
      handle: (H, P, String) => KRIO[R, String],
  ): ImplementedEndpoint[R] =
    ImplementedEndpoint(
      pathMatch = endpoint.path.paths == _,
      handleRequest = { request =>
        // TODO (KR) :
        ???
      },
    )

}

// --- No Response ---

extension [H, P](endpoint: Endpoint.None_None[H, P])
  @targetName("implement__None_None")
  def implement[R](handle: (H, P) => KRIO[R, Unit]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }

extension [H, P, RequestT](endpoint: Endpoint.Encoded_None[H, P, RequestT])
  @targetName("implement__Encoded_None")
  def implement[R](handle: (H, P, RequestT) => KRIO[R, Unit]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }

// TODO (KR) : File Request
extension [H, P](endpoint: Endpoint.File_None[H, P])
  @targetName("implement__File_None")
  def implement[R](handle: (H, P, Any) => KRIO[R, Unit]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }

// --- Encoded Response ---

extension [H, P, ResponseT](endpoint: Endpoint.None_Encoded[H, P, ResponseT])
  @targetName("implement__None_Encoded")
  def implement[R](handle: (H, P) => KRIO[R, ResponseT]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }

extension [H, P, RequestT, ResponseT](endpoint: Endpoint.Encoded_Encoded[H, P, RequestT, ResponseT])
  @targetName("implement__Encoded_Encoded")
  def implement[R](handle: (H, P, RequestT) => KRIO[R, ResponseT]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }

// TODO (KR) : File Request
extension [H, P, ResponseT](endpoint: Endpoint.File_None[H, P])
  @targetName("implement__File_Encoded")
  def implement[R](handle: (H, P, Any) => KRIO[R, ResponseT]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }

// --- File Response ---

// TODO (KR) : File Response
extension [H, P](endpoint: Endpoint.None_File[H, P])
  @targetName("implement__None_File")
  def implement[R](handle: (H, P) => KRIO[R, Any]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }

// TODO (KR) : File Response
extension [H, P, RequestT](endpoint: Endpoint.Encoded_File[H, P, RequestT])
  @targetName("implement__Encoded_File")
  def implement[R](handle: (H, P, RequestT) => KRIO[R, Any]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }

// TODO (KR) : File Request, File Response
extension [H, P](endpoint: Endpoint.File_File[H, P])
  @targetName("implement__File_File")
  def implement[R](handle: (H, P, Any) => KRIO[R, Any]): ImplementedEndpoint[R] = {
    // TODO (KR) :
    ???
  }
