package klib.utils

import scala.language.implicitConversions

import klib.Implicits._
import klib.fp.types._

final class Logger private (
    defaultLogTolerance: Logger.LogLevel with Logger.LogLevel.Tolerance,
    defaultFlags: Set[String],
    defaultIgnoredPackages: Set[String],
    defaultIndentString: String,
    defaultColorMode: Logger.ColorMode,
    sources: List[Logger.Source],
) { logger =>

  // =====| Create new logger with changes |=====

  private def copy(
      defaultLogTolerance: Logger.LogLevel with Logger.LogLevel.Tolerance = this.defaultLogTolerance,
      defaultFlags: Set[String] = this.defaultFlags,
      defaultIgnoredPackages: Set[String] = this.defaultIgnoredPackages,
      defaultIndentString: String = this.defaultIndentString,
      defaultColorMode: Logger.ColorMode = this.defaultColorMode,
      sources: List[Logger.Source] = this.sources,
  ): Logger =
    new Logger(
      defaultLogTolerance = defaultLogTolerance,
      defaultFlags = defaultFlags,
      defaultIndentString = defaultIndentString,
      defaultIgnoredPackages = defaultIgnoredPackages,
      defaultColorMode = defaultColorMode,
      sources = sources,
    )

  // --- Log Tolerance ---

  def withDefaultLogTolerance(defaultLogTolerance: Logger.LogLevel with Logger.LogLevel.Tolerance): Logger =
    copy(defaultLogTolerance = defaultLogTolerance)

  // --- Flags ---

  def withDefaultFlags(flags: String*): Logger =
    copy(defaultFlags = flags.toSet)

  def includingDefaultFlags(flags: String*): Logger =
    copy(defaultFlags = defaultFlags ++ flags)

  def excludingDefaultFlags(flags: String*): Logger =
    copy(defaultFlags = defaultFlags -- flags)

  // --- Ignored Packages ---

  def withIgnoredPackages(flags: String*): Logger =
    copy(defaultIgnoredPackages = flags.toSet)

  def includingIgnoredPackages(ignoredPackages: String*): Logger =
    copy(defaultIgnoredPackages = defaultIgnoredPackages ++ ignoredPackages)

  def excludingIgnoredPackages(ignoredPackages: String*): Logger =
    copy(defaultIgnoredPackages = defaultIgnoredPackages -- ignoredPackages)

  // --- Indent String ---

  def withDefaultIndentString(defaultIndentString: String): Logger =
    copy(defaultIndentString = defaultIndentString)

  // --- Color Mode ---

  def withDefaultColorMode(colorMode: Logger.ColorMode): Logger =
    copy(defaultColorMode = colorMode)

  // =====|  |=====

  object unsafeLog {

    def apply(
        event: Logger.Event,
        logTolerance: Maybe[Logger.LogLevel with Logger.LogLevel.Tolerance] = None,
        additionalFlags: Maybe[Set[String]] = None,
        additionalIgnoredPackages: Maybe[Set[String]] = None, // TODO (KR) : Possibly tweak how this is done (?)
        indentString: Maybe[String] = None,
        colorMode: Maybe[Logger.ColorMode] = None,
    ): Unit = {
      val logTol: Logger.LogLevel =
        logTolerance.getOrElse(logger.defaultLogTolerance)
      val flags: Set[String] =
        additionalFlags.cata(logger.defaultFlags ++ _, logger.defaultFlags)
      val ignoredPackages: Set[String] =
        additionalIgnoredPackages.cata(logger.defaultIgnoredPackages ++ _, logger.defaultIgnoredPackages)
      val idtStr: String =
        indentString.getOrElse(logger.defaultIndentString)
      val colMode: Logger.ColorMode =
        colorMode.getOrElse(logger.defaultColorMode)

      val ignoreStackTraceElements: List[Logger.IgnoreStackTraceElement] =
        ignoredPackages.toList.map(Logger.IgnoreStackTraceElement.ignorePackage)

      def conditionally(logLevel: Logger.LogLevel)(f: => Unit): Unit =
        if (logLevel.priority >= logTol.priority)
          f

      def handle(
          indent: String,
          event: Logger.Event,
      ): Unit = {
        def buildString(logLevel: Logger.LogLevel, message: Any): String = {
          val messageString =
            Maybe(message)
              .cata(
                _.toString.replaceAll("\n", s"${Logger.LogLevel.DisplayNameNewLine}:$indent"),
                "null",
              )
          s"${logLevel.tag(colMode)}:$indent$messageString"
        }

        event match {
          case Logger.Event.Compound(events) =>
            events.foreach(handle(indent, _))
          case Logger.Event.Log(logLevel, message) =>
            conditionally(logLevel) {
              val str = buildString(logLevel, message)
              sources.foreach(_.println(str))
            }
          case Logger.Event.Break(printIndent) =>
            sources.foreach(_.break(printIndent))
          case Logger.Event.Indented(event, by) =>
            handle(indent + idtStr * by.max(0), event)
          case event @ Logger.Event.LogThrowable(_, _, _, _) =>
            handle(
              indent,
              L(
                L.log(event.messageLevel, Maybe(event.throwable.getMessage).getOrElse(event.throwable.toString)),
                L.indented(
                  L(
                    L.log(event.stackTraceLevel, "stack-trace:"),
                    L.indented(
                      Logger.IgnoreStackTraceElement.trimmedTrace(event.throwable, ignoreStackTraceElements).map { ste =>
                        L.log(event.stackTraceLevel, ste.toString)
                      },
                    ),
                    event.causeEvent.map { eCause =>
                      L(
                        L.log(event.messageLevel, "cause:"),
                        L.indented(eCause),
                      )
                    },
                  ),
                ),
              ),
            )
          case Logger.Event.Raw(logLevel, str) =>
            def action(): Unit = {
              sources.foreach(_.print(str))
            }

            logLevel match {
              case Some(logLevel) =>
                conditionally(logLevel) {
                  action()
                }
              case None =>
                action()
            }
          case Logger.Event.RequireFlags(requiredFlags, event) =>
            if ((requiredFlags -- flags).isEmpty) {
              handle(indent, event())
            }
        }
      }

      handle(" ", event)
    }

    // ---  ---

    def simple(logLevel: Logger.LogLevel, message: Any): Unit =
      apply(Logger.Event.Log(logLevel, message))

    def never(message: Any): Unit = simple(Logger.LogLevel.Never, message)
    def debug(message: Any): Unit = simple(Logger.LogLevel.Debug, message)
    def detailed(message: Any): Unit = simple(Logger.LogLevel.Detailed, message)
    def info(message: Any): Unit = simple(Logger.LogLevel.Info, message)
    def important(message: Any): Unit = simple(Logger.LogLevel.Important, message)
    def warning(message: Any): Unit = simple(Logger.LogLevel.Warning, message)
    def error(message: Any): Unit = simple(Logger.LogLevel.Error, message)
    def fatal(message: Any): Unit = simple(Logger.LogLevel.Fatal, message)
    def always(message: Any): Unit = simple(Logger.LogLevel.Always, message)

    def throwable(
        throwable: Throwable,
        messageLevel: Logger.LogLevel = Logger.LogLevel.Fatal,
        stackTraceLevel: Logger.LogLevel = Logger.LogLevel.Debug,
    ): Unit = {
      def makeEvent(throwable: Throwable): Logger.Event.LogThrowable =
        Logger.Event.LogThrowable(
          messageLevel,
          stackTraceLevel,
          throwable,
          Maybe(throwable.getCause).map(makeEvent),
        )

      apply(makeEvent(throwable))
    }

    def break(printIndent: Boolean = true): Unit =
      apply(L.break(printIndent))

  }

  object log {

    def apply(
        event: Logger.Event,
        logTolerance: Maybe[Logger.LogLevel with Logger.LogLevel.Tolerance] = None,
        additionalFlags: Maybe[Set[String]] = None,
        additionalIgnoredPackages: Maybe[Set[String]] = None, // TODO (KR) : Possibly tweak how this is done (?)
        indentString: Maybe[String] = None,
    ): IO[Unit] =
      unsafeLog(
        event = event,
        logTolerance = logTolerance,
        additionalFlags = additionalFlags,
        additionalIgnoredPackages = additionalIgnoredPackages,
        indentString = indentString,
      ).pure[IO]

    // ---  ---

    def simple(logLevel: Logger.LogLevel, message: Any): IO[Unit] =
      apply(Logger.Event.Log(logLevel, message))

    def never(message: Any): IO[Unit] = simple(Logger.LogLevel.Never, message)
    def debug(message: Any): IO[Unit] = simple(Logger.LogLevel.Debug, message)
    def detailed(message: Any): IO[Unit] = simple(Logger.LogLevel.Detailed, message)
    def info(message: Any): IO[Unit] = simple(Logger.LogLevel.Info, message)
    def important(message: Any): IO[Unit] = simple(Logger.LogLevel.Important, message)
    def warning(message: Any): IO[Unit] = simple(Logger.LogLevel.Warning, message)
    def error(message: Any): IO[Unit] = simple(Logger.LogLevel.Error, message)
    def fatal(message: Any): IO[Unit] = simple(Logger.LogLevel.Fatal, message)
    def always(message: Any): IO[Unit] = simple(Logger.LogLevel.Always, message)

    def throwable(
        throwable: Throwable,
        messageLevel: Logger.LogLevel = Logger.LogLevel.Fatal,
        stackTraceLevel: Logger.LogLevel = Logger.LogLevel.Debug,
    ): IO[Unit] = {
      def makeEvent(throwable: Throwable): Logger.Event.LogThrowable =
        Logger.Event.LogThrowable(
          messageLevel,
          stackTraceLevel,
          throwable,
          Maybe(throwable.getCause).map(makeEvent),
        )

      apply(makeEvent(throwable))
    }

    def break(printIndent: Boolean = true): IO[Unit] =
      apply(L.break(printIndent))

  }

}

object Logger {

  sealed trait ColorMode
  object ColorMode {
    case object Extended extends ColorMode
    case object Simple extends ColorMode
    case object Colorless extends ColorMode
  }

  def apply(
      defaultLogTolerance: LogLevel with LogLevel.Tolerance,
      defaultFlags: Set[String] = Set.empty,
      defaultIgnorePackages: Set[String] = Set.empty,
      defaultIndentStr: String = "    ",
      defaultColorMode: ColorMode = ColorMode.Extended,
  ): Logger =
    new Logger(
      defaultLogTolerance = defaultLogTolerance,
      defaultFlags = defaultFlags,
      defaultIgnoredPackages = Set( // TODO (KR) : Maybe dont include these by default (?)
        "klib.fp.types",
        "klib.fp.typeclass",
        "scala.util.Try",
      ) ++ defaultIgnorePackages,
      defaultIndentString = defaultIndentStr,
      defaultColorMode = defaultColorMode,
      sources = List(
        new Source(
          printlnF = Console.out.println(_),
          printF = Console.out.print(_),
          breakManager = new Source.BreakManager,
        ),
      ),
    )

  sealed abstract class LogLevel(
      val priority: Int,
      val name: String,
      val displayName: String,
      val extendedColor: Color,
      val simpleColor: Color,
  ) {

    private def color(colorMode: ColorMode): Color =
      colorMode match {
        case ColorMode.Extended  => extendedColor
        case ColorMode.Simple    => simpleColor
        case ColorMode.Colorless => Color.Default
      }

    def tag(colorMode: ColorMode): String = {
      val paddedName = displayName.padTo(LogLevel.MaxDisplayNameLength, ' ')
      color(colorMode) match {
        case Color.Default =>
          s"[$paddedName]"
        case cmColor =>
          def ansi(color: Color): String = s"\u001b[${color.fgMod}m"
          s"[${ansi(cmColor)}$paddedName${ansi(Color.Default)}]"
      }
    }

    def toString(colorMode: ColorMode): String =
      s"$name${tag(colorMode)}($priority)"

    override def toString: String =
      toString(ColorMode.Extended)

  }
  object LogLevel {
    sealed trait Tolerance

    case object Never
        extends LogLevel(
          priority = 0,
          name = "Never",
          displayName = "NEVER",
          extendedColor = Color.Default, // TODO (KR) :
          simpleColor = Color.Default, // TODO (KR) :
        )

    case object Debug
        extends LogLevel(
          priority = 1,
          name = "Debug",
          displayName = "DEBUG",
          extendedColor = Color.RGB.fromHex(0x0277bd),
          simpleColor = Color.Named.Cyan,
        )
        with Tolerance

    case object Detailed
        extends LogLevel(
          priority = 2,
          name = "Detailed",
          displayName = "DETLD",
          extendedColor = Color.RGB.fromHex(0x66bb6a),
          simpleColor = Color.Named.Blue,
        )
        with Tolerance

    case object Info
        extends LogLevel(
          priority = 3,
          name = "Info",
          displayName = "INFO",
          extendedColor = Color.RGB.fromHex(0x1b5e20),
          simpleColor = Color.Named.Green,
        )
        with Tolerance

    case object Important
        extends LogLevel(
          priority = 4,
          name = "Important",
          displayName = "IMPRT",
          extendedColor = Color.RGB.fromHex(0x880e4f),
          simpleColor = Color.Named.Yellow,
        )
        with Tolerance

    case object Warning
        extends LogLevel(
          priority = 5,
          name = "Warning",
          displayName = "WARN",
          extendedColor = Color.RGB.fromHex(0xffff00),
          simpleColor = Color.Named.Yellow,
        )
        with Tolerance

    case object Error
        extends LogLevel(
          priority = 6,
          name = "Error",
          displayName = "ERROR",
          extendedColor = Color.RGB.fromHex(0xff3d00),
          simpleColor = Color.Named.Red,
        )
        with Tolerance

    case object Fatal
        extends LogLevel(
          priority = 7,
          name = "Fatal",
          displayName = "FATAL",
          extendedColor = Color.RGB.fromHex(0xd50000),
          simpleColor = Color.Named.Red,
        )
        with Tolerance

    case object Always
        extends LogLevel(
          priority = 8,
          name = "Always",
          displayName = "ALWYS",
          extendedColor = Color.Default, // TODO (KR) :
          simpleColor = Color.Default,
        )
        with Tolerance

    val All: List[LogLevel] =
      List(
        Never,
        Debug,
        Detailed,
        Info,
        Important,
        Warning,
        Error,
        Fatal,
        Always,
      )

    val AllTolerance: List[LogLevel with Tolerance] =
      List(
        Debug,
        Detailed,
        Info,
        Important,
        Warning,
        Error,
        Fatal,
        Always,
      )

    private[Logger] val MaxDisplayNameLength: Int =
      All.map(_.displayName.length).max

    private[Logger] val BreakDisplayName: String =
      s"[${" " * MaxDisplayNameLength}]"

    private[Logger] val DisplayNameNewLine: String =
      s"\n${" " * (MaxDisplayNameLength + 2)}"

  }

  sealed trait Event
  object Event {

    final case class Compound(events: List[Event]) extends Event
    final case class Log(logLevel: LogLevel, message: Any) extends Event
    final case class LogThrowable(
        messageLevel: LogLevel,
        stackTraceLevel: LogLevel,
        throwable: Throwable,
        causeEvent: Maybe[LogThrowable],
    ) extends Event
    final case class Raw(logLevel: Maybe[LogLevel], str: String) extends Event
    final case class Indented(event: Event, by: Int) extends Event
    final case class RequireFlags(flags: Set[String], event: () => Event) extends Event
    final case class Break(printIndent: Boolean) extends Event

    trait Implicits {

      implicit def maybeEventToEvent(mEvent: Maybe[Event]): Event =
        mEvent.cata(identity, L())

      implicit def listOfEventsToEvent(events: List[Event]): Event =
        L(events)

    }
    object Implicits extends Implicits

  }

  final class Source private[Logger] (
      printlnF: String => Unit,
      printF: String => Unit,
      breakManager: Source.BreakManager,
  ) {

    def println(str: String): Unit = {
      checkForBreak()
      printlnF(str)
    }

    def print(str: String): Unit = {
      checkForBreak()
      printF(str)
    }

    private def checkForBreak(): Unit = {
      if (breakManager.wantsToBreak) {
        printlnF(breakManager.printIndent ? s"${LogLevel.BreakDisplayName}:" | "")

        breakManager.wantsToBreak = false
        breakManager.printIndent = false
      }
    }

    def break(printIndent: Boolean): Unit = {
      breakManager.wantsToBreak = true
      breakManager.printIndent = printIndent || breakManager.printIndent
    }

  }

  object Source {

    private[Logger] class BreakManager {
      var wantsToBreak: Boolean = false
      var printIndent: Boolean = false
    }

  }

  trait IgnoreStackTraceElement {
    def ignore_?(ste: StackTraceElement): Boolean
  }
  object IgnoreStackTraceElement {

    def trimmedTrace(throwable: Throwable, ignored: List[IgnoreStackTraceElement]): List[StackTraceElement] =
      throwable.getStackTrace.toList.filter(st => !ignored.exists(_.ignore_?(st)))

    def ignorePackage(pkg: String): IgnoreStackTraceElement =
      _.getClassName.startsWith(pkg)

  }

}
