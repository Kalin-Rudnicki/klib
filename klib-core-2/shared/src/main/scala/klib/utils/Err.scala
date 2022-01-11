package klib.utils

import zio._

final case class Err(
    devMessage: String,
    userMessage: Option[String],
    stackTrace: Array[StackTraceElement], // TODO (KR) : Switch to ZIO stack-trace (?)
    cause: Option[Err],
) {

  def toLoggerEvent(errorLevel: Logger.LogLevel): URIO[RunMode, Logger.Event] =
    for {
      message <-
        ZIO.service[RunMode].map {
          case RunMode.Dev  => s"$devMessage [$userMessage]"
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
      Logger.println.info.event(message),
      Logger.Event(
        Err.stackTraceEvent(stackTrace),
        causeEvent,
      ),
    )

}
object Err {

  def apply(
      devMessage: String,
      userMessage: Option[String] = None,
      cause: Option[Err] = None,
  ): Err =
    Err(
      devMessage = devMessage,
      userMessage = userMessage,
      stackTrace = Thread.currentThread().getStackTrace,
      cause = cause,
    )

  def fromThrowable(throwable: Throwable, userMessage: Option[String] = None): Err =
    Err(
      devMessage = Option(throwable.getMessage).getOrElse(throwable.toString),
      userMessage = userMessage,
      stackTrace = throwable.getStackTrace,
      cause = Option(throwable.getCause).map(fromThrowable(_, None)),
    )

  // TODO (KR) : Improve?
  private def stackTraceEvent(stackTrace: Array[StackTraceElement]): Logger.Event =
    Logger.println.event.all(stackTrace.toList).requireLogLevel(Logger.LogLevel.Debug)

}
