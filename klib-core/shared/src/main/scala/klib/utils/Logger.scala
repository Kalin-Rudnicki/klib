package klib.utils

import scala.annotation.tailrec
import scala.language.dynamics
import scala.language.implicitConversions

import klib.Implicits._
import klib.fp.types._

final class Logger private (
    defaultLogTolerance: Logger.LogLevel with Logger.LogLevel.Tolerance,
    defaultFlags: Set[String],
    defaultIgnoredPackages: Set[String],
    defaultIndentString: String,
    defaultColorMode: Logger.ColorMode,
    sources: List[(Maybe[String], Logger.Source)],
    flagMap: Map[String, InfiniteSet[String]],
    initialIndent: Int,
) { logger =>

  private var currentIndent: Int = initialIndent

  private val baseFlags: InfiniteSet[String] =
    Logger.expandFlags(defaultFlags, flagMap)

  // =====| Create new logger with changes |=====

  private def copy(
      defaultLogTolerance: Logger.LogLevel with Logger.LogLevel.Tolerance = this.defaultLogTolerance,
      defaultFlags: Set[String] = this.defaultFlags,
      defaultIgnoredPackages: Set[String] = this.defaultIgnoredPackages,
      defaultIndentString: String = this.defaultIndentString,
      defaultColorMode: Logger.ColorMode = this.defaultColorMode,
      sources: List[(Maybe[String], Logger.Source)] = this.sources,
      flagMap: Map[String, InfiniteSet[String]] = this.flagMap,
      initialIndent: Int = this.currentIndent,
  ): Logger =
    new Logger(
      defaultLogTolerance = defaultLogTolerance,
      defaultFlags = defaultFlags,
      defaultIndentString = defaultIndentString,
      defaultIgnoredPackages = defaultIgnoredPackages,
      defaultColorMode = defaultColorMode,
      sources = sources,
      flagMap = flagMap,
      initialIndent = initialIndent,
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

  // --- Flag Map ---

  def withFlagMap(flagMap: Map[String, InfiniteSet[String]]): Logger =
    copy(
      flagMap = (this.flagMap.toList ++ flagMap.toList)
        .groupMap(_._1)(_._2)
        .map {
          case (key, value) =>
            key -> InfiniteSet.union(value: _*)
        },
    )

  def withMappedFlag(flag: String, mapsTo: InfiniteSet[String]): Logger =
    copy(
      flagMap = flagMap.updated(
        flag,
        flagMap.get(flag).toMaybe match {
          case Some(prevFlags) => prevFlags | mapsTo
          case None            => mapsTo
        },
      ),
    )

  def withMappedFlagInclusive(flag: String, flags: String*): Logger =
    withMappedFlag(flag, InfiniteSet.Inclusive(flags.toSet))

  def withMappedFlagExclusive(flag: String, flags: String*): Logger =
    withMappedFlag(flag, InfiniteSet.Exclusive(flags.toSet))

  // --- Sources ---

  def withSource(source: Logger.Source, name: Maybe[String] = None): Logger =
    copy(
      sources = this.sources.appended((name, source)),
    )

  def withoutSources(sourceNames: String*): Logger =
    copy(
      sources = sources.filter(_._1.cata(sourceNames.contains, true)),
    )

  // =====|  |=====

  object unsafeLog {

    // NOTE : Make sure if parameters are added here, they are added to log.apply as well
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
      val flags: InfiniteSet[String] =
        additionalFlags.cata(flags => Logger.expandFlags(logger.defaultFlags | flags, flagMap), baseFlags)
      val ignoredPackages: Set[String] =
        additionalIgnoredPackages.cata(logger.defaultIgnoredPackages ++ _, logger.defaultIgnoredPackages)
      val idtStr: String =
        indentString.getOrElse(logger.defaultIndentString)
      val colMode: Logger.ColorMode =
        colorMode.getOrElse(logger.defaultColorMode)

      val ignoreStackTraceElements: List[Logger.IgnoreStackTraceElement] =
        ignoredPackages.toList.map(Logger.IgnoreStackTraceElement.ignorePackage)

      // REMOVE : ...
      println(s"flags: $flags")

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
              .toStringNull(_.toString.replaceAll("\n", s"${Logger.LogLevel.DisplayNameNewLine}:$indent"))
          s"${logLevel.tag(colMode)}:$indent$messageString"
        }

        event match {
          case Logger.Event.Compound(events) =>
            events.foreach(handle(indent, _))
          case Logger.Event.Log(logLevel, message) =>
            conditionally(logLevel) {
              val str = buildString(logLevel, message)
              sources.foreach(_._2.println(str))
            }
          case Logger.Event.Break(printIndent) =>
            sources.foreach(_._2.break(printIndent))
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
              sources.foreach(_._2.print(str))
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
            if (requiredFlags.forall(flags.contains))
              handle(indent, event())
          case Logger.Event.IndentBy(delta) =>
            logger.currentIndent = (logger.currentIndent + delta).max(0)
          case Logger.Event.SetIndent(to) =>
            logger.currentIndent = to.max(0)
        }
      }

      handle(" " + idtStr * logger.currentIndent, event)
    }

    // ---  ---

    def simple(logLevel: Logger.LogLevel, message: Any, flags: String*): Unit =
      apply(L.requireFlags(flags: _*)(L.log(logLevel, message)))

    def never(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Never, message, flags: _*)
    def debug(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Debug, message, flags: _*)
    def detailed(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Detailed, message, flags: _*)
    def info(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Info, message, flags: _*)
    def important(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Important, message, flags: _*)
    def warning(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Warning, message, flags: _*)
    def error(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Error, message, flags: _*)
    def fatal(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Fatal, message, flags: _*)
    def always(message: Any, flags: String*): Unit = simple(Logger.LogLevel.Always, message, flags: _*)

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

    object indent {
      def by(delta: Int): Unit = apply(L.indent.by(delta))
      def to(to: Int): Unit = apply(L.indent.to(to))

      def byFor(delta: Int = 1)(exec: => Unit): Unit = {
        indent.by(delta)
        exec
        indent.by(-delta)
      }
    }

  }

  object log {

    def apply(
        event: Logger.Event,
        logTolerance: Maybe[Logger.LogLevel with Logger.LogLevel.Tolerance] = None,
        additionalFlags: Maybe[Set[String]] = None,
        additionalIgnoredPackages: Maybe[Set[String]] = None, // TODO (KR) : Possibly tweak how this is done (?)
        indentString: Maybe[String] = None,
        colorMode: Maybe[Logger.ColorMode] = None,
    ): IO[Unit] =
      unsafeLog(
        event = event,
        logTolerance = logTolerance,
        additionalFlags = additionalFlags,
        additionalIgnoredPackages = additionalIgnoredPackages,
        indentString = indentString,
        colorMode = colorMode,
      ).pure[IO]

    // ---  ---

    def simple(logLevel: Logger.LogLevel, message: Any, flags: String*): IO[Unit] =
      apply(L.requireFlags(flags: _*)(L.log(logLevel, message)))

    def never(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Never, message, flags: _*)
    def debug(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Debug, message, flags: _*)
    def detailed(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Detailed, message, flags: _*)
    def info(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Info, message, flags: _*)
    def important(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Important, message, flags: _*)
    def warning(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Warning, message, flags: _*)
    def error(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Error, message, flags: _*)
    def fatal(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Fatal, message, flags: _*)
    def always(message: Any, flags: String*): IO[Unit] = simple(Logger.LogLevel.Always, message, flags: _*)

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

    object indent {
      def by(delta: Int): IO[Unit] = apply(L.indent.by(delta))
      def to(to: Int): IO[Unit] = apply(L.indent.to(to))

      def byFor(delta: Int = 1)(exec: IO[Unit]): IO[Unit] =
        indent
          .by(delta)
          .bracket { _ =>
            exec
          } { _ =>
            indent.by(-delta)
          }
    }

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
      flagMap: Map[String, InfiniteSet[String]] = Map.empty,
      initialIndent: Int = 0,
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
        (
          "STDOUT".some,
          new Source(
            printlnF = Console.out.println(_),
            printF = Console.out.print(_),
            sourceState = new Source.SourceState,
          ),
        ),
      ),
      flagMap = flagMap,
      initialIndent = initialIndent,
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

  sealed trait Event {

    def requireFlags(flags: String*): Event =
      L.requireFlags(flags: _*)(this)

  }
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
    final case class IndentBy(delta: Int) extends Event
    final case class SetIndent(to: Int) extends Event

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
      sourceState: Source.SourceState,
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
      if (sourceState.wantsToBreak) {
        printlnF(sourceState.printIndent ? s"${LogLevel.BreakDisplayName}:" | "")

        sourceState.wantsToBreak = false
        sourceState.printIndent = false
      }
    }

    def break(printIndent: Boolean): Unit = {
      sourceState.wantsToBreak = true
      sourceState.printIndent = printIndent || sourceState.printIndent
    }

  }

  object Source {

    private[Logger] class SourceState {
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

  // =====|  |=====

  private[klib] def expandFlags(
      flags: Set[String],
      flagMap: Map[String, InfiniteSet[String]],
  ): InfiniteSet[String] = {
    def lookupFlag(flag: String): InfiniteSet[String] =
      flagMap.getOrElse(flag, InfiniteSet.empty) | InfiniteSet(flag)

    def expandInclusive(flags: Set[String]): InfiniteSet[String] = {
      @tailrec
      def loop(
          current: Set[String],
          seen: Set[String],
          track: InfiniteSet[String],
      ): InfiniteSet[String] = {
        val unseen = current &~ seen
        if (unseen.isEmpty)
          track
        else {
          val split =
            unseen.partitionMap { f =>
              lookupFlag(f) match {
                case InfiniteSet.Inclusive(explicit) => scala.Right(explicit)
                case InfiniteSet.Exclusive(explicit) => scala.Left(expandExclusive(explicit))
              }
            }

          // ...
          loop(
            split._2.flatten,
            seen | current,
            InfiniteSet.union(
              List(
                track :: Nil,
                split._1.toList.map(InfiniteSet.Exclusive(_)),
                split._2.toList.map(InfiniteSet.Inclusive(_)),
              ).flatten: _*,
            ),
          )
        }
      }

      loop(
        flags,
        Set.empty,
        InfiniteSet.empty,
      )
    }

    def expandExclusive(flags: Set[String]): Set[String] = {
      @tailrec
      def loop(
          current: Set[String],
          seen: Set[String],
          exclusive: Set[String],
      ): Set[String] = {
        val unseen = current &~ seen
        if (unseen.isEmpty)
          exclusive
        else {
          val filtered =
            current.flatMap { f =>
              lookupFlag(f) match {
                case InfiniteSet.Inclusive(explicit) => (f, explicit).someOpt
                case InfiniteSet.Exclusive(_)        => scala.None
              }
            }

          loop(
            filtered.flatMap(_._2),
            seen | current,
            exclusive | filtered.map(_._1),
          )
        }
      }

      loop(
        flags,
        Set.empty,
        Set.empty,
      )
    }

    expandInclusive(flags)
  }

}
