package klib.utils

import scala.annotation.tailrec

import cats.syntax.option._
import zio._

final class Logger private (
    defaultLogTolerance: Logger.LogLevel with Logger.LogLevel.Tolerance,
    defaultFlags: Set[String],
    defaultIndentString: String,
    defaultColorMode: Logger.ColorMode,
    sources: List[Logger.Source],
    flagMap: Map[String, InfiniteSet[String]],
    indent: Ref[Int],
) {

  private val baseFlags: InfiniteSet[String] =
    Logger.expandFlags(defaultFlags, flagMap)

}
object Logger {

  final class Source private (
      name: String,
      state: Ref[Source.State],
      ops: UIO[Source.Ops],
  ) {

    // TODO (KR) :

  }
  object Source {

    def apply(
        name: String,
        initialState: State,
        ops: UIO[Ops],
    ): UIO[Source] =
      for {
        state <- Ref.make(initialState)
      } yield new Source(
        name = name,
        state = state,
        ops = ops,
      )

    final case class State(
    )

    trait Ops {
      def print(any: Any): UIO[Unit]
      def println(any: Any): UIO[Unit]
      def log(any: Any): UIO[Unit]
    }

  }

  // =====| ColorMode |=====

  sealed trait ColorMode
  object ColorMode {
    case object Extended extends ColorMode
    case object Simple extends ColorMode
    case object Colorless extends ColorMode
  }

  // =====| LogLevel |=====

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

  // =====| Event |=====

  sealed trait Event
  object Event {

    final case class Compound(events: List[Event]) extends Event
    final case class Print(logLevel: Option[LogLevel], str: String) extends Event
    final case class Println(logLevel: Option[LogLevel], str: String) extends Event
    final case class Log(logLevel: LogLevel, message: Any) extends Event
    final case class LogThrowable(
        messageLevel: LogLevel,
        stackTraceLevel: LogLevel,
        throwable: Throwable,
        causeEvent: Option[LogThrowable],
    ) extends Event
    final case class Indented(event: Event, by: Int) extends Event
    final case class RequireFlags(flags: Set[String], event: () => Event) extends Event
    final case class Break(printIndent: Boolean) extends Event
    final case class IndentBy(delta: Int) extends Event
    final case class SetIndent(to: Int) extends Event

  }

  // =====| Helpers |=====

  private[Logger] def expandFlags(
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
                case InfiniteSet.Inclusive(explicit) => (f, explicit).some
                case InfiniteSet.Exclusive(_)        => None
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
