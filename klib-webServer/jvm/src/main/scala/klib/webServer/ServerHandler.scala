package klib.webServer

import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

import klib.Implicits._
import klib.utils._

final class ServerHandler(matcher: RouteMatcher, logger: Logger) extends AbstractHandler {

  override def handle(
      target: String,
      baseRequest: Request,
      request: HttpServletRequest,
      response: HttpServletResponse,
  ): Unit = {

    // TODO (KR) :
    ???
  }

}
