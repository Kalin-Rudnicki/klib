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
  val getEnv: KTask[Env]
  val titleF: Page.TitleF[Env]
  val widget: AVWidget[A, Env, Any]
  val handleA: A => SKTask[List[Raise.StandardOrUpdate[Env]]]

  private final def renderAnd(renderer: VDomActions.Renderer, runtime: Runtime[Executable.Env])(and: String => KTask[Unit]): KTask[Unit] =
    for {
      env <- getEnv
      envRef <- Ref.Synchronized.make(env)

      raiseHandler = RaiseHandler.root[A, Env](renderer, envRef, widget, handleA, titleF, runtime)
      newVDom = widget.elements(raiseHandler, env)
      _ <- renderer.render(titleF(env), newVDom)
    } yield ()

  private[web] final def push(renderer: VDomActions.Renderer, runtime: Runtime[Executable.Env]): KTask[Unit] =
    renderAnd(renderer, runtime) { title =>
      ZIO.kAttempt("Unable to push state to window history")(window.history.pushState(null, title, url))
    }

  private[web] final def replace(renderer: VDomActions.Renderer, runtime: Runtime[Executable.Env]): KTask[Unit] =
    renderAnd(renderer, runtime) { title =>
      ZIO.kAttempt("Unable to replace state in window history")(window.history.replaceState(null, title, url))
    }

  private[web] final def replaceNoTrace(renderer: VDomActions.Renderer, runtime: Runtime[Executable.Env]): KTask[Unit] =
    renderAnd(renderer, runtime) { _ => ZIO.unit }

}
object Page {

  opaque type TitleF[Env] = Either[String, Env => String]
  extension [Env](titleF: TitleF[Env]) {
    def apply(env: Env): String = titleF.fold(identity, _(env))
  }

  object builder {

    def apply(url: String): Builder1 =
      Builder1(url)

    final class Builder1 private[builder] (
        url: String,
    ) {

      def getEnv[Env](getEnv: KTask[Env]): Builder2[Env] =
        Builder2(url, getEnv)

      def constEnv[Env](env: Env): Builder2[Env] =
        Builder2(url, ZIO.succeed(env))

    }

    final class Builder2[Env] private[builder] (
        url: String,
        getEnv: KTask[Env],
    ) {

      def constTitle(title: String): Builder3[Env] =
        Builder3(url, getEnv, title.asLeft)

      def titleF(f: Env => String): Builder3[Env] =
        Builder3(url, getEnv, f.asRight)

    }

    final class Builder3[Env] private[builder] (
        url: String,
        getEnv: KTask[Env],
        titleF: Either[String, Env => String],
    ) {

      def body[A](widget: AVWidget[A, Env, Any]): Builder4[Env, A] =
        Builder4(url, getEnv, titleF, widget)

    }

    final class Builder4[Env, A] private[builder] (
        url: String,
        getEnv: KTask[Env],
        titleF: Either[String, Env => String],
        widget: AVWidget[A, Env, Any],
    ) { self =>

      def handleA(_handleA: A => SKTask[List[Raise.StandardOrUpdate[Env]]]): Page =
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
