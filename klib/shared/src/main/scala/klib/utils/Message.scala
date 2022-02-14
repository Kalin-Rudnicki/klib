package klib.utils

import cats.syntax.option.*
import zio.*

final case class Message(
    devMessage: String,
    userMessage: Option[String],
    stackTrace: Array[StackTraceElement], // TODO (KR) : Switch to ZIO stack-trace (?)
    cause: Option[Message],
) {

  def toLoggerEvent(errorLevel: Logger.LogLevel): URIO[RunMode, Logger.Event] =
    for {
      message <-
        ZIO.service[RunMode].map {
          case RunMode.Dev  => s"$devMessage${userMessage.fold("")(m => s" [$m]")}"
          case RunMode.User => userMessage.getOrElse("Unexpected Error")
        }
      causeEvent <-
        cause match {
          case Some(cause) =>
            cause.toLoggerEvent(errorLevel).map { event =>
              Logger
                .Event(
                  Logger.println.event("Cause:").requireLogLevel(errorLevel),
                  event.indented(1),
                )
                .indented(1)
            }
          case None =>
            ZIO.succeed(Logger.Event.empty)
        }
    } yield Logger.Event(
      Logger.println.fatal.event(message),
      Logger
        .Event(
          Message.stackTraceEvent(stackTrace),
          causeEvent,
        )
        .indented(1),
    )

}
object Message {

  inline def unexpected(devMessage: String, cause: Option[Message] = None): Message =
    Message(
      devMessage = devMessage,
      userMessage = None,
      stackTrace = Thread.currentThread().getStackTrace,
      cause = cause,
    )

  inline def withUserMessage(devMessage: String, userMessage: String, cause: Option[Message] = None): Message =
    Message(
      devMessage = devMessage,
      userMessage = userMessage.some,
      stackTrace = Thread.currentThread().getStackTrace,
      cause = cause,
    )

  inline def same(message: String, cause: Option[Message] = None): Message =
    Message(
      devMessage = message,
      userMessage = message.some,
      stackTrace = Thread.currentThread().getStackTrace,
      cause = cause,
    )

  def fromThrowable(throwable: Throwable, userMessage: Option[String] = None): Message =
    Message(
      devMessage = Option(throwable.getMessage).getOrElse(throwable.toString),
      userMessage = userMessage,
      stackTrace = throwable.getStackTrace,
      cause = Option(throwable.getCause).map(fromThrowable(_, None)),
    )

  inline def shouldNeverHappen: Message =
    Message(
      devMessage = "This should never happen...",
      userMessage = None,
      stackTrace = Thread.currentThread().getStackTrace,
      cause = None,
    )

  inline def ??? : Message = unexpected("Unimplemented")

  // TODO (KR) : Improve?
  private def stackTraceEvent(stackTrace: Array[StackTraceElement]): Logger.Event =
    Logger.println.event.all(stackTrace.toList).requireLogLevel(Logger.LogLevel.Debug)

}
