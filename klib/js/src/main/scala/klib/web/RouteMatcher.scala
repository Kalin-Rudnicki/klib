package klib.web

import cats.data.NonEmptyList
import cats.syntax.option.*
import org.scalajs.dom.URLSearchParams
import org.scalajs.dom.window
import scala.annotation.tailrec
import zio.*

import klib.fp.typeclass.DecodeFromString
import klib.utils.*

final case class RouteMatcher[T](private val attemptMatch: (List[String], ParamMap) => Option[(List[String], T)]) {

  def /:(path: String): RouteMatcher[T] =
    RouteMatcher.path(path).flatMap(_ => this)

  def as[T2](f: => T2): RouteMatcher[T2] =
    map(_ => f)

  def map[T2](f: T => T2): RouteMatcher[T2] =
    RouteMatcher { (path, params) =>
      attemptMatch(path, params)
        .map { (remainingPath, t) => (remainingPath, f(t)) }
    }

  def flatMap[T2](f: T => RouteMatcher[T2]): RouteMatcher[T2] =
    RouteMatcher { (path, params) =>
      attemptMatch(path, params)
        .flatMap { (remainingPath, t) => f(t).attemptMatch(remainingPath, params) }
    }

  def runMatch(path: List[String], params: ParamMap): Option[T] =
    attemptMatch(path, params) match {
      case Some((Nil, t)) => t.some
      case _              => None
    }

  def attemptToLoadPage(renderer: VDomActions.Renderer, runtime: Runtime[Executable.BaseEnv])(implicit ev: T <:< Page): STaskM[Unit] =
    for {
      pathname <- ZIOM.attempt(window.location.pathname)
      search <- ZIOM.attempt(window.location.search)

      pathnames = pathname.split("/").toList.filter(_.nonEmpty)
      params <- ZIOM.attempt { new URLSearchParams(search).toList.map { t => (t._1, t._2) }.toMap }
      paramMap = ParamMap("param", params)

      _ <-
        runMatch(pathnames, paramMap) match {
          case Some(page) => ev(page).replaceNoTrace(renderer, runtime)
          case None =>
            val pathStr: String = pathnames.mkString("/")
            val paramStr: String = params.map { (k, v) => s"$k=$v" }.mkString("&")
            Logger.println.error(s"Unable to load page: $pathStr${if (paramStr.nonEmpty) "?" else ""}$paramStr")
        }
    } yield ()

}
object RouteMatcher {

  def pure[T](t: => T): RouteMatcher[T] =
    RouteMatcher((paths, _) => (paths, t).some)

  def oneOf[T](routeMatchers: RouteMatcher[T]*): RouteMatcher[T] =
    RouteMatcher { (path, params) =>
      @tailrec
      def loop(routeMatchers: List[RouteMatcher[T]]): Option[(List[String], T)] =
        routeMatchers match {
          case head :: tail =>
            head.attemptMatch(path, params) match {
              case res @ Some(_) => res
              case None          => loop(tail)
            }
          case Nil => None
        }

      loop(routeMatchers.toList)
    }

  def path(path: String): RouteMatcher[Unit] =
    RouteMatcher { (paths, params) =>
      paths match {
        case head :: tail if (head == path) => (tail, ()).some
        case _                              => None
      }
    }

  def paths(path0: String, paths: String*): RouteMatcher[Unit] =
    (path0 :: paths.toList)
      .foldRight(RouteMatcher.pure(()))(_ /: _)

  def consumeEntirePath: RouteMatcher[Unit] =
    RouteMatcher((_, _) => (Nil, ()).some)

  def param[P: DecodeFromString](name: String): RouteMatcher[P] =
    RouteMatcher { (paths, params) =>
      params.getParamO(name) match {
        case Right(Some(p)) => (paths, p).some
        case _              => None
      }
    }

}
