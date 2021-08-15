package klib.utils

import scala.language.implicitConversions

import klib.Implicits._
import klib.fp.types._
import klib.utils.Logger.{helpers => L}

final class Logger private (
    defaultLogTolerance: Logger.LogLevel with Logger.LogLevel.Tolerance,
    defaultFlags: Set[String],
    defaultIgnoredPackages: Set[String],
    defaultIndentString: String,
    defaultColorMode: Logger.ColorMode,
    sources: List[Logger.Source],
) {

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

  def withColorMode(colorMode: Logger.ColorMode): Logger =
    copy(defaultColorMode = colorMode)

  // =====|  |=====

  def unsafeLog(
      event: Logger.Event,
      logTolerance: Maybe[Logger.LogLevel with Logger.LogLevel.Tolerance] = None,
      additionalFlags: Maybe[Set[String]] = None,
      additionalIgnoredPackages: Maybe[Set[String]] = None, // TODO (KR) : Possibly tweak how this is done (?)
      indentString: Maybe[String] = None,
      colorMode: Maybe[Logger.ColorMode] = None,
  ): Unit = {
    val logTol: Logger.LogLevel =
      logTolerance.getOrElse(this.defaultLogTolerance)
    val flags: Set[String] =
      additionalFlags.cata(this.defaultFlags ++ _, this.defaultFlags)
    val ignoredPackages: Set[String] =
      additionalIgnoredPackages.cata(this.defaultIgnoredPackages ++ _, this.defaultIgnoredPackages)
    val idtStr: String =
      indentString.getOrElse(this.defaultIndentString)
    val colMode: Logger.ColorMode =
      colorMode.getOrElse(this.defaultColorMode)

    val ignoreStackTraceElements: List[Logger.IgnoreStackTraceElement] =
      ignoredPackages.toList.map(Logger.IgnoreStackTraceElement.ignorePackage)

    def conditionally(logLevel: Logger.LogLevel)(f: => Unit): Unit =
      if (logLevel.priority >= logTol.priority)
        f

    def handle(
        indent: String,
        event: Logger.Event,
    ): Unit = {
      import Logger.helpers.Implicits._

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
        case logThrowable @ Logger.Event.LogThrowable(_, _, _, _) =>
          def expand(event: Logger.Event.LogThrowable): Logger.Event =
            L(
              L.log(event.messageLevel, s"${event.throwable.getMessage} (${event.throwable.getClass.getSimpleName})"),
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
            )

          handle(
            indent,
            expand(logThrowable),
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

  object helpers {

    trait Implicits {

      implicit def maybeEventToEvent(mEvent: Maybe[Event]): Event =
        mEvent.cata(identity, helpers())

      implicit def listOfEventsToEvent(events: List[Event]): Event =
        helpers(events)

    }
    object Implicits extends Implicits

    def apply(events: List[Event]): Event =
      Event.Compound(events)

    def apply(events: Event*): Event =
      helpers(events.toList)

    object log {

      def apply(logLevel: LogLevel, message: Any): Event =
        Event.Log(logLevel, message)

      def never(message: Any): Event = log(LogLevel.Never, message)
      def debug(message: Any): Event = log(LogLevel.Debug, message)
      def detailed(message: Any): Event = log(LogLevel.Detailed, message)
      def info(message: Any): Event = log(LogLevel.Info, message)
      def important(message: Any): Event = log(LogLevel.Important, message)
      def warning(message: Any): Event = log(LogLevel.Warning, message)
      def error(message: Any): Event = log(LogLevel.Error, message)
      def fatal(message: Any): Event = log(LogLevel.Fatal, message)
      def always(message: Any): Event = log(LogLevel.Always, message)

      def throwable(
          throwable: Throwable,
          messageLevel: LogLevel = LogLevel.Fatal,
          stackTraceLevel: LogLevel = LogLevel.Debug,
      ): Event = {
        def makeEvent(throwable: Throwable): Event.LogThrowable =
          Event.LogThrowable(
            messageLevel,
            stackTraceLevel,
            throwable,
            Maybe(throwable.getCause).map(makeEvent),
          )

        makeEvent(throwable)
      }

    }

    def raw(str: String, logLevel: Maybe[LogLevel] = None): Event =
      Event.Raw(logLevel, str)

    object ansi {

      def apply(afterEscape: String, logLevel: Maybe[LogLevel] = None): Event =
        raw(s"$AnsiEscapeString$afterEscape", logLevel)

      /**
        * NOTE : (row, column) is 1-based, so (1,1) is the top-left
        */
      def cursorPos(row: Int, column: Int, logLevel: Maybe[LogLevel] = None): Event =
        ansi(s"$row;${column}H", logLevel)

      object cursor {

        def up(numLines: Int, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${numLines}A", logLevel)

        def down(numLines: Int, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${numLines}B", logLevel)

        def forward(numLines: Int, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${numLines}C", logLevel)

        def back(numLines: Int, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${numLines}D", logLevel)

        def nextLine(numLines: Int = 1, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${numLines}E", logLevel)

        def previousLine(numLines: Int = 1, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${numLines}F", logLevel)

      }

      object scroll {

        def up(numLines: Int, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${numLines}S", logLevel)

        def down(numLines: Int, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${numLines}T", logLevel)

      }

      object clearLine {

        sealed abstract class Mod(private[clearLine] val i: Int)
        object Mod {
          case object CursorToEndOfLine extends Mod(0)
          case object CursorToBeginningOfLine extends Mod(1)
          case object EntireLine extends Mod(2)
        }

        def apply(mod: Mod = Mod.EntireLine, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${mod.i}K", logLevel)

      }

      object clearScreen {

        sealed abstract class Mod(private[clearScreen] val i: Int)
        object Mod {
          case object CursorToEndOfScreen extends Mod(0)
          case object CursorToBeginningOfScreen extends Mod(1)
          case object EntireScreenAndMoveCursorToTopLeft extends Mod(2)
          case object EntireScreenAndClearScrollbackBuffer extends Mod(3)
        }

        def apply(mod: Mod = Mod.EntireScreenAndMoveCursorToTopLeft, logLevel: Maybe[LogLevel] = None): Event =
          ansi(s"${mod.i}J", logLevel)
      }

      // TODO (KR) : Device Status Report (?)

    }

    def indented(event: Event, by: Int = 1): Event =
      Event.Indented(event, by)

    def requireFlags(flags: String*)(event: => Event): Event =
      Event.RequireFlags(flags.toSet, () => event)

    def break(printIndent: Boolean = true): Event =
      Event.Break(printIndent)

  }

}
