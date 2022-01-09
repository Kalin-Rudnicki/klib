package klib.utils

import scala.annotation.tailrec

import cats.data.*
import cats.syntax.option.*
import cats.syntax.list.*
import zio.*

final class Logger private (
    flags: InfiniteSet[String],
    defaultIndent: String,
    colorMode: Logger.ColorMode,
    sources: List[Logger.Source],
    indents: Ref[List[String]], // TODO (KR) : Maybe this should be on the Source (?)
) { logger =>

  private def withIndent[T](f: String => UIO[T]): UIO[T] =
    for {
      currentIndents <- indents.get
      t <- f(currentIndents.headOption.getOrElse(""))
    } yield t

  def execute(event: Logger.Event): UIO[Unit] = {
    def execOnSource(source: Logger.Source): UIO[Any] = {
      def handle(
          logLevel: Option[Logger.LogLevel],
          ops: Logger.Source.Ops,
          event: Logger.Event,
      ): UIO[Any] = {
        def output(f: String => UIO[Unit]): UIO[Unit] =
          withIndent { indent =>
            for {
              _ <- source.queuedBreak.get.flatMap {
                case Some(break) =>
                  break.print match {
                    case Logger.Event.Break.Print.Nothing =>
                      ops.println("")
                    case Logger.Event.Break.Print.Tag =>
                      ops.println(Logger.formatMessage(colorMode, logLevel, "", ""))
                    case Logger.Event.Break.Print.TagAndIndent =>
                      ops.println(Logger.formatMessage(colorMode, logLevel, indent, ""))
                  }
                case None =>
                  UIO.unit
              }
              _ <- f(indent)
            } yield ()
          }

        event match {
          case Logger.Event.Compound(events) =>
            ZIO.foreach(events)(handle(logLevel, ops, _))
          case newBreak @ Logger.Event.Break(_, _) =>
            source.queuedBreak.update {
              case Some(currentBreak) =>
                def copied: Option[Logger.Event.Break] = currentBreak.copy(print = newBreak.print).some

                currentBreak.`type` match {
                  case Logger.Event.Break.Type.Open =>
                    newBreak.`type` match {
                      case Logger.Event.Break.Type.Open   => copied
                      case Logger.Event.Break.Type.Normal => copied
                      case Logger.Event.Break.Type.Close  => None
                    }
                  case Logger.Event.Break.Type.Normal => copied
                  case Logger.Event.Break.Type.Close  => copied
                }
              case None =>
                newBreak.some
            }
          case Logger.Event.Bracket(_1, _2, _3) =>
            handle(logLevel, ops, _1).bracket(release = _ => handle(logLevel, ops, _3))(use = _ => handle(logLevel, ops, _2))
          case Logger.Event.Print(message) =>
            output { indent =>
              ops.print(Logger.formatMessage(colorMode, logLevel, indent, message))
            }
          case Logger.Event.Println(message) =>
            output { indent =>
              ops.println(Logger.formatMessage(colorMode, logLevel, indent, message))
            }
          case Logger.Event.Log(message) =>
            output { _ =>
              ops.log(message)
            }
          case Logger.Event.PushIndent(indent) =>
            indents.update { currentIndents =>
              Logger.calcIndent(currentIndents.headOption.getOrElse(""), defaultIndent, indent) :: currentIndents
            }
          case Logger.Event.PopIndent =>
            indents.update(_.drop(1))
          case Logger.Event.RequireFlags(requiredFlags, event) =>
            if (requiredFlags.forall(flags.contains)) handle(logLevel, ops, event())
            else UIO.unit
          case Logger.Event.RequireLogLevel(logLevel, event) =>
            if (logLevel.priority >= source.logTolerance.priority) handle(logLevel.some, ops, event())
            else UIO.unit
        }
      }

      source.withOps(handle(None, _, event).unit)
    }

    // TODO (KR) : foreachPar?
    ZIO.foreach(sources)(execOnSource).unit
  }

}
object Logger {

  def apply(
      flags: Set[String],
      flagMap: Map[String, InfiniteSet[String]],
      defaultIndent: String,
      colorMode: Logger.ColorMode,
      sources: List[UIO[Source]],
      initialIndents: List[String],
  ): UIO[Logger] =
    for {
      sources <- ZIO.foreach(sources)(identity)
      indents <- Ref.make(initialIndents)
    } yield new Logger(
      flags = expandFlags(flags, flagMap),
      defaultIndent = defaultIndent,
      colorMode = colorMode,
      sources = sources,
      indents = indents,
    )

  // =====| API |=====

  // =====| Source |=====

  abstract class Source(
      val name: String,
      val logTolerance: LogLevel with LogLevel.Tolerance,
      val queuedBreak: Ref[Option[Event.Break]],
  ) {

    type Src <: Source.Ops

    protected val acquire: UIO[Src]
    protected def release(src: Src): UIO[Unit]

    def withOps(f: Source.Ops => UIO[Unit]): UIO[Unit] =
      acquire.bracket(release)(f)

  }
  object Source {

    final case class Ops(
        print: Any => UIO[Unit],
        println: Any => UIO[Unit],
        log: Any => UIO[Unit],
    )

    // ---  ---

    def stdOut(
        logTolerance: LogLevel with LogLevel.Tolerance,
        initialQueuedBreak: Option[Event.Break] = None,
    ): UIO[Source] =
      for {
        queuedBreak <- Ref.make(initialQueuedBreak)
        ops = Ops(
          print = msg => ZIO { scala.Console.print(msg) }.orDie,
          println = msg => ZIO { scala.Console.println(msg) }.orDie,
          log = msg => ZIO { scala.Console.println(msg) }.orDie,
        )
      } yield new Source("StdOut", logTolerance, queuedBreak) {
        override type Src = ops.type
        override val acquire: UIO[Src] = UIO(ops)
        override def release(src: Src): UIO[Unit] = UIO.unit
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

    def toString(colorMode: ColorMode): String =
      s"$name${LogLevel.tag(this.some, colorMode)}($priority)"

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

    // ---  ---

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

    // ---  ---

    private[Logger] val MaxDisplayNameLength: Int =
      All.map(_.displayName.length).max

    private[Logger] val EmptyDisplayTag: String =
      "[" + (" " * MaxDisplayNameLength) + "]"

    private[Logger] def tag(logLevel: Option[LogLevel], colorMode: ColorMode): String =
      logLevel match {
        case Some(logLevel) =>
          def ansi(color: Color): String = s"$AnsiEscapeString${color.fgMod}m"

          def color(colorMode: ColorMode): Color =
            colorMode match {
              case ColorMode.Extended  => logLevel.extendedColor
              case ColorMode.Simple    => logLevel.simpleColor
              case ColorMode.Colorless => Color.Default
            }

          val paddedName = logLevel.displayName.padTo(LogLevel.MaxDisplayNameLength, ' ')
          color(colorMode) match {
            case Color.Default =>
              s"[$paddedName]"
            case cmColor =>
              s"[${ansi(cmColor)}$paddedName${ansi(Color.Default)}]"
          }
        case None =>
          EmptyDisplayTag
      }

  }

  // =====| Event |=====

  sealed trait Event
  object Event {
    final case class Compound(events: List[Event]) extends Event
    final case class Bracket(_1: Event, _2: Event, _3: Event) extends Event
    final case class Break(`type`: Break.Type, print: Break.Print) extends Event
    object Break {
      enum Type { case Open, Normal, Close }
      enum Print { case Nothing, Tag, TagAndIndent }
    }

    final case class Print(message: Any) extends Event
    final case class Println(message: Any) extends Event
    final case class Log(message: Any) extends Event

    final case class PushIndent(indent: Either[Int, String]) extends Event
    case object PopIndent extends Event

    final case class RequireFlags(flags: Set[String], event: () => Event) extends Event
    final case class RequireLogLevel(logLevel: LogLevel, event: () => Event) extends Event
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
        if (unseen.isEmpty) track
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
        if (unseen.isEmpty) exclusive
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

  private[Logger] def calcIndent(currentIndent: String, defaultIndent: String, indent: Either[Int, String]): String =
    indent match {
      case Left(by)      => currentIndent + (defaultIndent * by)
      case Right(indent) => currentIndent + indent
    }

  private[Logger] def formatMessage(
      colorMode: ColorMode,
      logLevel: Option[LogLevel],
      indent: String,
      message: Any,
  ): String = {
    val tmp = message.toString
    val msg =
      if (tmp.contains('\n')) tmp.replaceAll("\n", s"\n${" " * (LogLevel.MaxDisplayNameLength + 2)}: $indent")
      else tmp
    s"${LogLevel.tag(logLevel, colorMode)}: $indent$msg"
  }

}
