package klib

import java.io.PrintStream

final class Logger private (
    initialLogTolerance: Logger.LogLevel,
    initialFlags: Set[String],
    indentStr: String,
) {

  // =====| Internal State |=====

  private val _logTolerance: Logger.LogLevel = initialLogTolerance
  private val _flags: Set[String] = initialFlags

  // =====| Logging |=====

  final class Source private[Logger] (
      src: PrintStream,
      indent: Int,
  ) {

    private val effIdt = indent.max(0)
    private val idtStr = indentStr * effIdt

    def indented(by: Int = 1)(srcF: Source => Unit): Unit =
      srcF(new Source(src, indent + by))

    // ...

    def log(logLevel: Logger.LogLevel, objs: Any*): Unit =
      if (logLevel.priority >= _logTolerance.priority) {
        val tag = logLevel.tag
        val nlStr = s"${Logger.LogLevel.DisplayNameNewLine}: $idtStr"
        objs.foreach { obj =>
          src.println(s"$tag: $idtStr${obj.toString.replaceAllLiterally("\n", nlStr)}")
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

  def apply(flags: String*)(srcF: Source => Unit): Unit =
    if ((flags.toSet &~ _flags).isEmpty)
      srcF(new Source(Console.out, 0))

}

object Logger {

  def apply(
      logTolerance: LogLevel,
      flags: Set[String] = Set.empty,
      indentStr: String = "    ",
  ): Logger =
    new Logger(
      initialLogTolerance = logTolerance,
      initialFlags = flags,
      indentStr = indentStr,
    )

  sealed abstract class LogLevel(
      val priority: Int,
      val name: String,
      val displayName: String,
      val color: Color,
  ) {

    def tag: String =
      color match {
        case Color.Default =>
          s"[$displayName]"
        case _ =>
          def ansi(color: Color): String = s"\u001b[${color.fgMod}m"
          s"[${ansi(color)}$displayName${ansi(Color.Default)}]"
      }

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

    private[Logger] val MaxDisplayNameLength: Int =
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
      ).map(_.displayName.length).max

    private[Logger] val DisplayNameNewLine: String =
      s"\n${" " * (MaxDisplayNameLength + 2)}"

  }

}
