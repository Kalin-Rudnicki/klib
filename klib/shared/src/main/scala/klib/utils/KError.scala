package klib.utils

import cats.data.NonEmptyList
import cats.syntax.option.*
import scala.annotation.targetName
import zio.{Chunk, Trace}

type EitherError[A] = Either[KError, A]
type EitherErrorNEL[A] = Either[NonEmptyList[KError], A]

// TODO (KR) : Add a concept of retry-ability when it becomes useful.
sealed abstract class KError(
    userMessage: String,
    internalMessage: String,
    cause: Option[Throwable],
    trace: Chunk[Trace],
) extends Throwable {

  final val _userMessage: String = userMessage
  final val _internalMessage: String = internalMessage
  final val _cause: Option[Throwable] = cause
  final val _trace: Chunk[Trace] = trace

  final lazy val fullInternalMessage: String =
    List[Option[(String, String)]](
      ("User Message", userMessage).some,
      Option.when(internalMessage != userMessage)(("Internal Message", internalMessage)),
      cause.map(c => (s"Cause[${KError.throwableClassName(c)}]", KError.throwableMessage(c))),
      ("Exception Trace", KError.formatExceptionTrace(getStackTrace)).some,
      ("ZIO Trace", KError.formatZIOTrace(trace)).some,
    ).flatten
      .map { (label, message) =>
        message.split("\n").mkString(s"\n  $label:\n      > ", "\n        ", "")
      }
      .mkString("--- KError ---", "", "")

  final def formatMessage(runMode: RunMode): String =
    runMode match {
      case RunMode.Dev  => fullInternalMessage
      case RunMode.Prod => userMessage
    }

  final def formatEvent(runMode: RunMode, errorLevel: Logger.LogLevel): Logger.Event =
    runMode match {
      case RunMode.Dev  => KError.internalLogEvent(this, errorLevel)
      case RunMode.Prod => Logger.println.event(userMessage).requireLogLevel(errorLevel)
    }

  override final def getMessage: String = userMessage

  override final def getCause: Throwable = cause.orNull

  override final def toString: String = fullInternalMessage

}
object KError {

  // TODO (KR) : Remove
  def main(args: Array[String]): Unit = {
    List[KError](
      KError.UserError("User"),
      KError.UserError("User", "Internal"),
      KError.UserError("User", "Internal\nAnd a second line"),
      KError.UserError("User", "Internal", new RuntimeException),
      KError.UserError("User", "Internal", new RuntimeException("With a message")),
      KError.UserError("User", new RuntimeException),
      KError.Unexpected("Woa..."),
      KError.Unexpected("Woa...", KError.SystemFailure("How did this happen!!!")),
    ).foreach { error =>
      println()
      println(error.fullInternalMessage)
    }
  }

  // =====| Error Types |=====

  enum ErrorType {
    case User
    case Unexpected
    case SystemFailure
    case ExternalService(serviceName: String)
  }

  /**
    * Used when the user of the program gives some bad input.
    */
  final class UserError private (
      userMessage: String,
      internalMessage: String,
      cause: Option[Throwable],
      trace: Chunk[Trace],
  ) extends KError(userMessage, internalMessage, cause, trace)
  object UserError extends ErrorBuilder4[UserError](new UserError(_, _, _, _))

  /**
    * Used when you want to start the error message with "WTF, why is this happening"
    * - Map#get(_).thisShouldAlwaysBeThere
    * - List#toNel.thisShouldAlwaysBeNonEmpty
    */
  final class Unexpected private (
      internalMessage: String,
      cause: Option[Throwable],
      trace: Chunk[Trace],
  ) extends KError(genericUserMessage, s"This should never happen...\n$internalMessage", cause, trace)
  object Unexpected extends ErrorBuilder3[Unexpected](new Unexpected(_, _, _))

  // TODO (KR) : Not sure what to do about 'SystemFailure' / 'ExternalService'.
  //           : At a later date, put some more thought into possibly adding more options,
  //           : and/or modifying those types

  final class SystemFailure private (
      internalMessage: String,
      cause: Option[Throwable],
      trace: Chunk[Trace],
  ) extends KError(genericUserMessage, internalMessage, cause, trace)
  object SystemFailure extends ErrorBuilder3[SystemFailure](new SystemFailure(_, _, _))

  final class ExternalService private (
      serviceName: String,
      internalMessage: String,
      cause: Option[Throwable],
      trace: Chunk[Trace],
  ) extends KError(s"There was a problem with an external service ($serviceName)", internalMessage, cause, trace)
  object ExternalService {

    final class Builder private[ExternalService] (serviceName: String) extends ErrorBuilder3[ExternalService](new ExternalService(serviceName, _, _, _))

    def apply(serviceName: String): Builder = Builder(serviceName)

  }

  /**
    * Analogous to the scala predef '???', except instead of throwing, it gives you a concrete error type.
    */
  @targetName("Unimplemented")
  final class ??? private (
      functionalityVerb: String,
      trace: Chunk[Trace],
  ) extends KError(
        s"The ability to '$functionalityVerb' is not yet supported",
        s"Encountered an unimplemented block of code: '$functionalityVerb'",
        None,
        trace,
      )
  object ??? {
    def apply(functionalityVerb: String)(implicit trace: Trace): ??? = new ???(functionalityVerb, Chunk.single(trace))
    def apply(functionalityVerb: String, trace: Chunk[Trace]): ??? = new ???(functionalityVerb, trace)
  }

  // =====| Utils |=====

  // TODO (KR) : Come up with a better name & message?
  private val genericUserMessage: String = "There was a non-handleable error in the system"

  final case class WrappedKErrors(errors: NonEmptyList[KError]) extends Throwable {
    override def getMessage: String = errors.toString.mkString("\n")
  }

  def unwrap(throwable: Throwable): NonEmptyList[KError] =
    unwrapOr(throwable, Unexpected("Expected to unwrap KError/WrappedKErrors", _))

  def unwrapOr(throwable: Throwable, or: Throwable => KError): NonEmptyList[KError] =
    throwable match {
      case WrappedKErrors(errors) => errors
      case kError: KError         => NonEmptyList.one(kError)
      case throwable              => NonEmptyList.one(or(throwable))
    }

  private def internalLogEvent(throwable: Throwable, errorLevel: Logger.LogLevel): Logger.Event =
    throwable match {
      case kError: KError =>
        val sections: List[(String, Logger.LogLevel, Logger.Event)] =
          List[Option[(String, Logger.LogLevel, Logger.Event)]](
            ("User Message", errorLevel, Logger.println.event(kError._userMessage)).some,
            Option.when(kError._internalMessage != kError._userMessage)(("Internal Message", errorLevel, Logger.println.event(kError._internalMessage))),
            kError._cause.map(c => (s"Cause[${throwableClassName(c)}]", Logger.LogLevel.Detailed, internalLogEvent(c, errorLevel))),
            ("Exception Trace", Logger.LogLevel.Debug, Logger.println.event(formatExceptionTrace(kError.getStackTrace))).some,
            ("ZIO Trace", Logger.LogLevel.Debug, Logger.println.event(formatZIOTrace(kError._trace))).some,
          ).flatten

        Logger.Event(
          Logger.println.event("--- KError ---").requireLogLevel(errorLevel),
          Logger.Event
            .Compound(
              sections.map[Logger.Event] { (label, logLevel, event) =>
                Logger
                  .Event(
                    Logger.println.event(s"$label:"),
                    event.indented(1),
                  )
                  .requireLogLevel(logLevel)
              },
            )
            .indented(1),
        )
      case WrappedKErrors(errors) =>
        Logger.Event.Compound(errors.toList.map(internalLogEvent(_, errorLevel)))
      case throwable =>
        Logger.println.error.event(throwableMessage(throwable))
    }

  private def throwableClassName(throwable: Throwable): String =
    throwable match {
      case kError: KError => s"KError.${kError.getClass.getSimpleName}"
      case throwable =>
        val name = throwable.getClass.getSimpleName
        // TODO (KR) : Figure out what the best option is here.
        if (name.nonEmpty) name else throwable.getClass.getName
    }

  // TODO (KR) : Do something else?
  private def throwableMessage(throwable: Throwable): String =
    Option(throwable.getMessage).getOrElse(throwable.toString)

  // TODO (KR) : Make prettier?
  private def formatExceptionTrace(trace: Array[StackTraceElement]): String = trace.mkString("\n")

  // TODO (KR) : Make prettier?
  private def formatZIOTrace(trace: Chunk[Trace]): String = trace.mkString("\n\n")

  // =====| Builders |=====

  abstract class ErrorBuilder3[E](build: (String, Option[Throwable], Chunk[Trace]) => E) {
    def apply(internalMessage: String)(implicit trace: Trace): E = build(internalMessage, None, Chunk.single(trace))
    def apply(internalMessage: String, cause: Throwable)(implicit trace: Trace): E = build(internalMessage, cause.some, Chunk.single(trace))
    def apply(internalMessage: String, cause: Option[Throwable])(implicit trace: Trace): E = build(internalMessage, cause, Chunk.single(trace))

    def apply(internalMessage: String, trace: Chunk[Trace]): E = build(internalMessage, None, trace)
    def apply(internalMessage: String, cause: Throwable, trace: Chunk[Trace]): E = build(internalMessage, cause.some, trace)
    def apply(internalMessage: String, cause: Option[Throwable], trace: Chunk[Trace]): E = build(internalMessage, cause, trace)
  }

  abstract class ErrorBuilder4[E](build: (String, String, Option[Throwable], Chunk[Trace]) => E) {
    def apply(userMessage: String)(implicit trace: Trace): E = build(userMessage, userMessage, None, Chunk.single(trace))
    def apply(userMessage: String, cause: Throwable)(implicit trace: Trace): E = build(userMessage, userMessage, cause.some, Chunk.single(trace))
    def apply(userMessage: String, cause: Option[Throwable])(implicit trace: Trace): E = build(userMessage, userMessage, cause, Chunk.single(trace))
    def apply(userMessage: String, internalMessage: String)(implicit trace: Trace): E = build(userMessage, internalMessage, None, Chunk.single(trace))
    def apply(userMessage: String, internalMessage: String, cause: Throwable)(implicit trace: Trace): E = build(userMessage, internalMessage, cause.some, Chunk.single(trace))
    def apply(userMessage: String, internalMessage: String, cause: Option[Throwable])(implicit trace: Trace): E = build(userMessage, internalMessage, cause, Chunk.single(trace))

    def apply(userMessage: String, trace: Chunk[Trace]): E = build(userMessage, userMessage, None, trace)
    def apply(userMessage: String, cause: Throwable, trace: Chunk[Trace]): E = build(userMessage, userMessage, cause.some, trace)
    def apply(userMessage: String, cause: Option[Throwable], trace: Chunk[Trace]): E = build(userMessage, userMessage, cause, trace)
    def apply(userMessage: String, internalMessage: String, trace: Chunk[Trace]): E = build(userMessage, internalMessage, None, trace)
    def apply(userMessage: String, internalMessage: String, cause: Throwable, trace: Chunk[Trace]): E = build(userMessage, internalMessage, cause.some, trace)
    def apply(userMessage: String, internalMessage: String, cause: Option[Throwable], trace: Chunk[Trace]): E = build(userMessage, internalMessage, cause, trace)
  }

}
