package klib.utils

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.option.*
import zio.*

import klib.utils.*

type EitherError[A] = Either[KError[Nothing], A]

sealed trait ErrorBuilder[ErrOut[_], MessageOut[_], ThrowableOut] {

  protected def buildErr[E](err: SingleError.Err[E]): ErrOut[E]
  protected def buildMessage[E](message: SingleError.Message[E]): MessageOut[E]
  protected def buildThrowable(throwable: SingleError.Throwable): ThrowableOut

  // --- Err ---

  def makeError[E](err: E, cause: Option[KError[E]]): ErrOut[E] =
    buildErr(SingleError.Err(err, Thread.currentThread.getStackTrace, cause))

  inline def error[E](err: E): ErrOut[E] =
    makeError(err, None)

  inline def error[E](err: E, cause: KError[E]): ErrOut[E] =
    makeError(err, cause.some)

  // --- Message ---

  object message {

    // Core

    def make[E](devMessage: String, prodMessage: String, cause: Option[KError[E]]): MessageOut[E] =
      buildMessage(SingleError.Message(devMessage, prodMessage, Thread.currentThread.getStackTrace, cause))

    inline def apply(devMessage: String, prodMessage: String): MessageOut[Nothing] =
      message.make(devMessage, prodMessage, None)

    inline def apply[E](devMessage: String, prodMessage: String, cause: KError[E]): MessageOut[E] =
      message.make(devMessage, prodMessage, cause.some)

    // Helpers

    inline def unexpected(devMessage: String): MessageOut[Nothing] =
      message(devMessage, "An unexpected error occurred")

    inline def unexpected[E](devMessage: String, cause: KError[E]): MessageOut[E] =
      message(devMessage, "An unexpected error occurred", cause)

    inline def same(msg: String): MessageOut[Nothing] =
      message(msg, msg)

    inline def same[E](msg: String, cause: KError[E]): MessageOut[E] =
      message(msg, msg, cause)

    inline def shouldNeverHappen: MessageOut[Nothing] =
      message.unexpected("This should never happen")

    inline def ??? : MessageOut[Nothing] =
      message.unexpected("??? : Unimplemented")

  }

  // --- Throwable ---

  def throwable(throwable: java.lang.Throwable): ThrowableOut =
    buildThrowable(SingleError.Throwable(throwable))

}

sealed abstract class ErrorThrowable(
    _message: String,
    _stackTrace: Option[Array[StackTraceElement]],
    _cause: => Option[Throwable],
) extends Throwable(_message, _cause.orNull) {
  _stackTrace.foreach(setStackTrace)
}
object ErrorThrowable {

  final case class Err(err: SingleError.Err[_]) extends ErrorThrowable(err.error.toString, err.stackTrace.some, err.cause.map(fromError))
  final case class Message(msg: SingleError.Message[_]) extends ErrorThrowable(s"${msg.devMessage} [${msg.prodMessage}]", msg.stackTrace.some, msg.cause.map(fromError))
  final case class Multiple(errors: NonEmptyList[Throwable]) extends ErrorThrowable(s"Multiple errors occurred:\n - ${errors.toList.map(_.getMessage).mkString("\n - ")}", None, None)

  def fromSingleError(error: SingleError[Any]): Throwable =
    error match {
      case err: SingleError.Err[_]          => Err(err)
      case msg: SingleError.Message[_]      => Message(msg)
      case throwable: SingleError.Throwable => throwable.throwable
    }

  def fromError(error: KError[Any]): Throwable =
    error.toEither match {
      case Left(error)   => fromSingleError(error)
      case Right(errors) => Multiple(errors.map(fromSingleError))
    }

}

sealed trait SingleError[+E] {
  final def toKError: KError[E] = KError(this)
}
object SingleError extends ErrorBuilder[SingleError.Err, SingleError.Message, SingleError.Throwable] {

  // =====| Types |=====

  final case class Err[+E](
      error: E,
      stackTrace: Array[StackTraceElement],
      cause: Option[KError[E]],
  ) extends SingleError[E]

  final case class Message[+E](
      devMessage: String,
      prodMessage: String,
      stackTrace: Array[StackTraceElement],
      cause: Option[KError[E]],
  ) extends SingleError[E]

  final case class Throwable(
      throwable: java.lang.Throwable,
  ) extends SingleError[Nothing]

  // =====| Builders |=====

  override protected def buildErr[E](e: SingleError.Err[E]): SingleError.Err[E] = e
  override protected def buildMessage[E](e: SingleError.Message[E]): SingleError.Message[E] = e
  override protected def buildThrowable(e: SingleError.Throwable): SingleError.Throwable = e

  // =====| Helpers |=====

  private def convertStackTrace(stackTrace: Array[StackTraceElement]): Logger.Event =
    Logger.println.event.all(stackTrace.toList).requireLogLevel(Logger.LogLevel.Debug)

  private def convertCause[E](
      cause: Option[KError[E]],
      runMode: RunMode,
      convertE: E => String,
  ): Logger.Event =
    cause match {
      case None =>
        Logger.Event.empty
      case Some(cause) =>
        Logger
          .Event(
            Logger.println.event("Cause:"),
            KError.convertError(cause, runMode, convertE).indented(1),
          )
          .requireLogLevel(Logger.LogLevel.Debug)
    }

  private[utils] def convertSingleError[E](
      singleError: SingleError[E],
      runMode: RunMode,
      convertE: E => String,
  ): Logger.Event =
    singleError match {
      case SingleError.Err(error, stackTrace, cause) =>
        Logger.Event(
          Logger.println.event("KError:"),
          Logger.withIndent.event(1)(
            Logger.println.event(convertE(error)),
            convertStackTrace(stackTrace),
            convertCause(cause, runMode, convertE),
          ),
        )
      case SingleError.Message(devMessage, prodMessage, stackTrace, cause) =>
        Logger.Event(
          Logger.println.event(
            runMode match {
              case RunMode.Dev =>
                if (devMessage != prodMessage) s"$devMessage [$prodMessage]"
                else devMessage
              case RunMode.Prod => prodMessage
            },
          ),
          Logger.withIndent.event(1)(
            convertStackTrace(stackTrace),
            convertCause(cause, runMode, convertE),
          ),
        )
      case Throwable(throwable) =>
        Logger.Event(
          Logger.println.event("Throwable:"),
          Logger.withIndent.event(1)(
            Logger.println.event(Option(throwable.getMessage).getOrElse(throwable.toString)),
            convertStackTrace(throwable.getStackTrace),
            convertCause(Option(throwable.getCause).map(KError.throwable), runMode, convertE),
          ),
        )
    }

}

opaque type KError[+E] <: WrappedMultiple[SingleError[E]] = WrappedMultiple[SingleError[E]]
extension [E](error: KError[E]) {

  def causes(f: (SingleError.type, KError[E]) => SingleError[E]): KError[E] =
    KError(f(SingleError, error))

  def causesSpecific[O[_] <: SingleError[_]](f: (SingleError.type, KError[E]) => O[E]): O[E] =
    f(SingleError, error)

  def toLoggerEvent(errorLevel: Logger.LogLevel, convertE: E => String): URIO[RunMode, Logger.Event] =
    ZIO.service[RunMode].map { runMode =>
      KError.convertError(error, runMode, convertE).requireLogLevel(errorLevel)
    }

}
object KError extends ErrorBuilder[KError, KError, KError[Nothing]] {

  // =====| Builders |=====

  override protected def buildErr[E](e: SingleError.Err[E]): KError[E] = KError(e)
  override protected def buildMessage[E](e: SingleError.Message[E]): KError[E] = KError(e)
  override protected def buildThrowable(e: SingleError.Throwable): KError[Nothing] = KError(e)

  def apply[E](e0: SingleError[E], eN: SingleError[E]*): KError[E] =
    WrappedMultiple(e0, eN*)

  def apply[E](errors: NonEmptyList[SingleError[E]]): KError[E] =
    WrappedMultiple(errors.head, errors.tail*)

  def flatten[E](e0: KError[E], eN: KError[E]*): KError[E] =
    WrappedMultiple.flatten(e0, eN*)

  def flatten[E](errors: NonEmptyList[KError[E]]): KError[E] =
    WrappedMultiple.flatten(errors.head, errors.tail*)

  // =====| Typeclass Instances |=====

  implicit def semigroup[E]: Semigroup[KError[E]] =
    KError.flatten(_, _)

  // =====| Helpers |=====

  private[utils] def convertError[E](
      cause: KError[E],
      runMode: RunMode,
      convertE: E => String,
  ): Logger.Event =
    Logger.Event.all(
      cause.toNEL.toList.map(SingleError.convertSingleError(_, runMode, convertE)),
    )

}
