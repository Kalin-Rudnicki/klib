package klib.utils

import java.io.PrintStream

final class Logger private (
    flags: Set[String],
    src: Logger.Source,
) {

  // =====| Logging |=====

  def apply(flags: String*)(srcF: Logger.Source => Unit): Unit =
    if ((flags.toSet &~ this.flags).isEmpty)
      srcF(src)

  def withIndent(by: Int = 1): Logger =
    new Logger(
      flags = flags,
      src = src.withIndent(by),
    )

}

object Logger {

  def apply(
      logTolerance: LogLevel,
      flags: Set[String] = Set.empty,
      indentStr: String = "    ",
      indent: Int = 0,
  ): Logger =
    new Logger(
      flags = flags,
      src = new Source(
        src = Console.out,
        logTolerance = logTolerance,
        indentStr = indentStr,
        indent = indent,
        breakManager = new Source.BreakManager,
      ),
    )

  sealed abstract class LogLevel(
      val priority: Int,
      val name: String,
      val displayName: String,
      val color: Color,
  ) {

    def tag: String = {
      val paddedName = displayName.padTo(LogLevel.MaxDisplayNameLength, ' ')
      color match {
        case Color.Default =>
          s"[$paddedName]"
        case _ =>
          def ansi(color: Color): String = s"\u001b[${color.fgMod}m"
          s"[${ansi(color)}$paddedName${ansi(Color.Default)}]"
      }
    }

    override def toString: String =
      s"$name$tag($priority)"

  }

  object LogLevel {

    case object Never
        extends LogLevel(
          priority = 0,
          name = "Never",
          displayName = "NEVER",
          color = Color.Default, // TODO (KR) :
        )

    case object Debug
        extends LogLevel(
          priority = 1,
          name = "Debug",
          displayName = "DEBUG",
          color = Color.RGB(0x02, 0x77, 0xbd),
        )

    case object Detailed
        extends LogLevel(
          priority = 2,
          name = "Detailed",
          displayName = "DETLD",
          color = Color.RGB.fromHex(0x66bb6a),
        )

    case object Info
        extends LogLevel(
          priority = 3,
          name = "Info",
          displayName = "INFO",
          color = Color.RGB.fromHex(0x1b5e20),
        )

    case object Print
        extends LogLevel(
          priority = 4,
          name = "Print",
          displayName = "PRINT",
          color = Color.Default,
        )

    case object Important
        extends LogLevel(
          priority = 5,
          name = "Important",
          displayName = "IMPRT",
          color = Color.RGB.fromHex(0x880e4f),
        )

    case object Warning
        extends LogLevel(
          priority = 6,
          name = "Warning",
          displayName = "WARN",
          color = Color.RGB.fromHex(0xffff00),
        )

    case object Error
        extends LogLevel(
          priority = 7,
          name = "Error",
          displayName = "ERROR",
          color = Color.RGB.fromHex(0xff3d00),
        )

    case object Fatal
        extends LogLevel(
          priority = 8,
          name = "Fatal",
          displayName = "FATAL",
          color = Color.RGB.fromHex(0xd50000),
        )

    case object Always
        extends LogLevel(
          priority = 9,
          name = "Always",
          displayName = "ALWYS",
          color = Color.Default, // TODO (KR) :
        )

    val All: List[LogLevel] =
      List(
        Never,
        Debug,
        Detailed,
        Info,
        Print,
        Important,
        Warning,
        Error,
        Fatal,
        Always,
      )

    private[Logger] val MaxDisplayNameLength: Int =
      All.map(_.displayName.length).max

    private[Logger] val DisplayNameNewLine: String =
      s"\n${" " * (MaxDisplayNameLength + 2)}"

  }

  final class Source private[Logger] (
      src: PrintStream,
      logTolerance: LogLevel,
      indentStr: String,
      indent: Int,
      breakManager: Source.BreakManager,
  ) {

    private val effIdt = indent.max(0)
    private val idtStr = indentStr * effIdt
    private val newLineStr = s"${Logger.LogLevel.DisplayNameNewLine}: $idtStr"

    // ...

    def withIndent(by: Int = 1): Source =
      new Source(
        src = src,
        logTolerance = logTolerance,
        indentStr = indentStr,
        indent = indent + by,
        breakManager = breakManager,
      )

    def indented(by: Int = 1)(srcF: Source => Unit): Unit =
      srcF(withIndent(by))

    object ansi {

      private val esc = "\u001b["

      /**
        * NOTE : (row, column) is 1-based, so (1,1) is the top-left
        */
      def cursorPos(row: Int, column: Int): Unit =
        src.print(s"$esc$row;${column}H")

      def cursorUp(numLines: Int = 1): Unit =
        src.print(s"$esc${numLines}F")

      // TODO (KR) : Info about mod
      def clearLine(mod: Int = 0): Unit =
        src.print(s"$esc${mod}K")

      // TODO (KR) : Info about mod
      def clearScreen(mod: Int = 0): Unit =
        src.print(s"$esc${mod}J")

    }

    // ...

    def break: Unit = {
      if (breakManager.state) {
        src.println
        breakManager.state = false
      }
    }

    def exec(logLevel: LogLevel)(f: => Unit): Unit =
      if (logLevel.priority >= logTolerance.priority)
        f

    // ...

    def log(logLevel: Logger.LogLevel, objs: Any*): Unit =
      if (logLevel.priority >= logTolerance.priority) {
        val tag = logLevel.tag
        objs.foreach { obj =>
          breakManager.state = true
          src.println(s"$tag: $idtStr${obj.toString.replaceAllLiterally("\n", newLineStr)}")
        }
      }

    def never(objs: Any*): Unit =
      log(Logger.LogLevel.Never, objs: _*)

    def debug(objs: Any*): Unit =
      log(Logger.LogLevel.Debug, objs: _*)

    def detailed(objs: Any*): Unit =
      log(Logger.LogLevel.Detailed, objs: _*)

    def info(objs: Any*): Unit =
      log(Logger.LogLevel.Info, objs: _*)

    def print(objs: Any*): Unit =
      log(Logger.LogLevel.Print, objs: _*)

    def important(objs: Any*): Unit =
      log(Logger.LogLevel.Important, objs: _*)

    def warning(objs: Any*): Unit =
      log(Logger.LogLevel.Warning, objs: _*)

    def error(objs: Any*): Unit =
      log(Logger.LogLevel.Error, objs: _*)

    def fatal(objs: Any*): Unit =
      log(Logger.LogLevel.Fatal, objs: _*)

    def always(objs: Any*): Unit =
      log(Logger.LogLevel.Always, objs: _*)

  }

  object Source {

    private[Logger] class BreakManager {
      var state: Boolean = false
    }

  }

}
