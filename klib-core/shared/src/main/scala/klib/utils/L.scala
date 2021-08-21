package klib.utils

import klib.Implicits._
import klib.fp.types._

object L {

  def apply(events: List[Logger.Event]): Logger.Event =
    Logger.Event.Compound(events)

  def apply(events: Logger.Event*): Logger.Event =
    L(events.toList)

  object log {

    def apply(logLevel: Logger.LogLevel, message: Any): Logger.Event =
      Logger.Event.Log(logLevel, message)

    def never(message: Any): Logger.Event = log(Logger.LogLevel.Never, message)
    def debug(message: Any): Logger.Event = log(Logger.LogLevel.Debug, message)
    def detailed(message: Any): Logger.Event = log(Logger.LogLevel.Detailed, message)
    def info(message: Any): Logger.Event = log(Logger.LogLevel.Info, message)
    def important(message: Any): Logger.Event = log(Logger.LogLevel.Important, message)
    def warning(message: Any): Logger.Event = log(Logger.LogLevel.Warning, message)
    def error(message: Any): Logger.Event = log(Logger.LogLevel.Error, message)
    def fatal(message: Any): Logger.Event = log(Logger.LogLevel.Fatal, message)
    def always(message: Any): Logger.Event = log(Logger.LogLevel.Always, message)

    def throwable(
        throwable: Throwable,
        messageLevel: Logger.LogLevel = Logger.LogLevel.Fatal,
        stackTraceLevel: Logger.LogLevel = Logger.LogLevel.Debug,
    ): Logger.Event = {
      def makeEvent(throwable: Throwable): Logger.Event.LogThrowable =
        Logger.Event.LogThrowable(
          messageLevel,
          stackTraceLevel,
          throwable,
          Maybe(throwable.getCause).map(makeEvent),
        )

      makeEvent(throwable)
    }

  }

  def raw(str: String, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
    Logger.Event.Raw(logLevel, str)

  object ansi {

    def apply(afterEscape: String, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
      raw(s"$AnsiEscapeString$afterEscape", logLevel)

    /**
      * NOTE : (row, column) is 1-based, so (1,1) is the top-left
      */
    def cursorPos(row: Int, column: Int, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
      ansi(s"$row;${column}H", logLevel)

    object cursor {

      def up(numLines: Int, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${numLines}A", logLevel)

      def down(numLines: Int, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${numLines}B", logLevel)

      def forward(numLines: Int, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${numLines}C", logLevel)

      def back(numLines: Int, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${numLines}D", logLevel)

      def nextLine(numLines: Int = 1, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${numLines}E", logLevel)

      def previousLine(numLines: Int = 1, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${numLines}F", logLevel)

    }

    object scroll {

      def up(numLines: Int, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${numLines}S", logLevel)

      def down(numLines: Int, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${numLines}T", logLevel)

    }

    object clearLine {

      sealed abstract class Mod(private[clearLine] val i: Int)
      object Mod {
        case object CursorToEndOfLine extends Mod(0)
        case object CursorToBeginningOfLine extends Mod(1)
        case object EntireLine extends Mod(2)
      }

      def apply(mod: Mod = Mod.EntireLine, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
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

      def apply(mod: Mod = Mod.EntireScreenAndMoveCursorToTopLeft, logLevel: Maybe[Logger.LogLevel] = None): Logger.Event =
        ansi(s"${mod.i}J", logLevel)
    }

    // TODO (KR) : Device Status Report (?)

  }

  def indented(event: Logger.Event, by: Int = 1): Logger.Event =
    Logger.Event.Indented(event, by)

  def requireFlags(flags: String*)(event: => Logger.Event): Logger.Event =
    if (flags.isEmpty) event
    else Logger.Event.RequireFlags(flags.toSet, () => event)

  def break(printIndent: Boolean = true): Logger.Event =
    Logger.Event.Break(printIndent)

}
