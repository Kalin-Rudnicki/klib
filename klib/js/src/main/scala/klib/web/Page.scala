package klib.web

import cats.data.NonEmptyList
import cats.syntax.either.*
import javax.xml.crypto.Data
import org.scalajs.dom.window
import zio.*

import klib.utils.*

sealed trait Page {
  type Env
  type A
  val url: String
  val getEnv: TaskM[Env]
  val titleF: Either[String, Env => String]
  val widget: AVWidget[A, Env, Any]
  val handleA: A => STaskM[List[Raise.StandardOrUpdate[Env]]]

  private final def renderAnd(renderer: VDomActions.Renderer, runtime: Runtime[Executable.BaseEnv])(and: String => TaskM[Unit]): TaskM[Unit] =
    for {
      env <- getEnv
      envRef <- Ref.Synchronized.make(env)
      title =
        titleF match {
          case Right(f) => f(env)
          case Left(s)  => s
        }
      raiseHandler = RaiseHandler.root[A, Env](renderer, envRef, widget, handleA, titleF.toOption, runtime)
      newVDom = widget.elements(raiseHandler, env)
      _ <- renderer.render(newVDom)
      _ <- renderer.setPageTitle(title)
    } yield ()

  private[web] final def push(renderer: VDomActions.Renderer, runtime: Runtime[Executable.BaseEnv]): TaskM[Unit] =
    renderAnd(renderer, runtime) { title =>
      ZIOM.attempt(window.history.pushState(null, title, url))
    }

  private[web] final def replace(renderer: VDomActions.Renderer, runtime: Runtime[Executable.BaseEnv]): TaskM[Unit] =
    renderAnd(renderer, runtime) { title =>
      ZIOM.attempt(window.history.replaceState(null, title, url))
    }

  private[web] final def replaceNoTrace(renderer: VDomActions.Renderer, runtime: Runtime[Executable.BaseEnv]): TaskM[Unit] =
    renderAnd(renderer, runtime) { _ => ZIO.unit }

}
object Page {

  object builder {

    def apply(url: String): Builder1 =
      Builder1(url)

    final class Builder1 private[builder] (
        url: String,
    ) {

      def getEnv[Env](getEnv: TaskM[Env]): Builder2[Env] =
        Builder2(url, getEnv)

      def constEnv[Env](env: Env): Builder2[Env] =
        Builder2(url, ZIO.succeed(env))

    }

    final class Builder2[Env] private[builder] (
        url: String,
        getEnv: TaskM[Env],
    ) {

      def constTitle(title: String): Builder3[Env] =
        Builder3(url, getEnv, title.asLeft)

      def titleF(f: Env => String): Builder3[Env] =
        Builder3(url, getEnv, f.asRight)

    }

    final class Builder3[Env] private[builder] (
        url: String,
        getEnv: TaskM[Env],
        titleF: Either[String, Env => String],
    ) {

      def body[A](widget: AVWidget[A, Env, Any]): Builder4[Env, A] =
        Builder4(url, getEnv, titleF, widget)

    }

    final class Builder4[Env, A] private[builder] (
        url: String,
        getEnv: TaskM[Env],
        titleF: Either[String, Env => String],
        widget: AVWidget[A, Env, Any],
    ) { self =>

      def handleA(_handleA: A => STaskM[List[Raise.StandardOrUpdate[Env]]]): Page =
        new Page {
          type Env = self.Env
          type A = self.A
          val url = self.url
          val getEnv = self.getEnv
          val titleF = self.titleF
          val widget = self.widget
          val handleA = _handleA
        }

      inline def logA: Page =
        handleA(a => Logger.println.warning(s"Ignoring action: $a").as(Nil))

    }

  }

}
