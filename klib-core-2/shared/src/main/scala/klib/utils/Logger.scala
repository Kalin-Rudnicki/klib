package klib.utils

import java.io.BufferedWriter
import java.nio.file.OpenOption
import java.nio.file.StandardOpenOption

import scala.annotation.tailrec

import cats.data.*
import cats.syntax.option.*
import cats.syntax.list.*
import zio.*

final class Logger private (
    defaultIndent: String,
    sources: List[Logger.Source],
    flags: InfiniteSet[String],
    colorMode: Logger.ColorMode, // TODO (KR) : Move to Source
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
      ): UIO[Any] =
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
          case Logger.Event.Output(_type, message) =>
            withIndent { indent =>
              for {
                _ <- source.queuedBreak.get.flatMap {
                  case Some(break) =>
                    {
                      break.print match {
                        case Logger.Event.Break.Print.Nothing =>
                          ops.println("")
                        case Logger.Event.Break.Print.Tag =>
                          ops.println(Logger.formatMessage(colorMode, logLevel, "", ""))
                        case Logger.Event.Break.Print.TagAndIndent =>
                          ops.println(Logger.formatMessage(colorMode, logLevel, indent, ""))
                      }
                    } *> source.queuedBreak.set(None)
                  case None =>
                    UIO.unit
                }
                _ <- _type match {
                  case Logger.Event.Output.Type.Print   => ops.print(Logger.formatMessage(colorMode, logLevel, indent, message))
                  case Logger.Event.Output.Type.Println => ops.println(Logger.formatMessage(colorMode, logLevel, indent, message))
                  case Logger.Event.Output.Type.Log     => ops.log(message)
                }
              } yield ()
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

      source.withOps(handle(None, _, event).unit)
    }

    // TODO (KR) : foreachPar?
    ZIO.foreach(sources)(execOnSource).unit
  }

}
object Logger {

  def apply(
      defaultIndent: String,
      flags: Set[String],
      flagMap: Map[String, InfiniteSet[String]],
      sources: List[UIO[Logger.Source]],
      colorMode: Logger.ColorMode,
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

  def layer(
      defaultIndent: String,
      flags: Set[String],
      flagMap: Map[String, InfiniteSet[String]],
      sources: List[UIO[Logger.Source]],
      colorMode: Logger.ColorMode,
      initialIndents: List[String],
  ): ZLayer[Any, Nothing, Logger] =
    Logger(
      defaultIndent = defaultIndent,
      flags = flags,
      flagMap = flagMap,
      sources = sources,
      colorMode = colorMode,
      initialIndents = initialIndents,
    ).toLayer

  // =====| API |=====

  object execute {

    def apply(event: Event): URIO[Logger, Unit] =
      ZIO.service[Logger].flatMap(_.execute(event))
    def apply(event0: Event, event1: Event, eventN: Event*): URIO[Logger, Unit] =
      execute(Event.Compound(event0 :: event1 :: eventN.toList))
    def all(events: List[Event]): URIO[Logger, Unit] =
      execute(Event.Compound(events))

  }

  // --- Output ---

  sealed abstract class Out(`type`: Event.Output.Type) { o =>

    object event {

      def apply(message: Any): Event =
        Event.Output(`type`, message)
      def apply(message0: Any, message1: Any, messageN: Any*): Event =
        Event.Compound((message0 :: message1 :: messageN.toList).map(Event.Output(`type`, _)))
      def all(messages: List[Any]): Event =
        Event.Compound(messages.map(Event.Output(`type`, _)))

    }

    def apply(message: Any): URIO[Logger, Unit] =
      execute(o.event(message))
    def apply(message0: Any, message1: Any, messageN: Any*): URIO[Logger, Unit] =
      execute(o.event.all(message0 :: message1 :: messageN.toList))
    def all(messages: List[Any]): URIO[Logger, Unit] =
      execute(o.event.all(messages))

    sealed abstract class WithLogLevel(logLevel: LogLevel) { wll =>

      object event {

        def apply(message: Any): Event =
          Event.Output(`type`, message).requireLogLevel(logLevel)
        def apply(message0: Any, message1: Any, messageN: Any*): Event =
          Event.Compound((message0 :: message1 :: messageN.toList).map(Event.Output(`type`, _))).requireLogLevel(logLevel)
        def all(messages: List[Any]): Event =
          Event.Compound(messages.map(Event.Output(`type`, _))).requireLogLevel(logLevel)

      }

      def apply(message: Any): URIO[Logger, Unit] =
        execute(wll.event(message))
      def apply(message0: Any, message1: Any, messageN: Any*): URIO[Logger, Unit] =
        execute(wll.event.all(message0 :: message1 :: messageN.toList))
      def all(messages: List[Any]): URIO[Logger, Unit] =
        execute(wll.event.all(messages))

    }

    object never extends WithLogLevel(LogLevel.Never)
    object debug extends WithLogLevel(LogLevel.Debug)
    object detailed extends WithLogLevel(LogLevel.Detailed)
    object info extends WithLogLevel(LogLevel.Info)
    object important extends WithLogLevel(LogLevel.Important)
    object warning extends WithLogLevel(LogLevel.Warning)
    object error extends WithLogLevel(LogLevel.Error)
    object fatal extends WithLogLevel(LogLevel.Fatal)
    object always extends WithLogLevel(LogLevel.Always)

  }

  object print extends Out(Event.Output.Type.Print)
  object println extends Out(Event.Output.Type.Println)
  object log extends Out(Event.Output.Type.Log)

  // --- Indent ---

  object indent {

    object event {

      def apply(by: Int): Event =
        Event.PushIndent(Left(by))
      def apply(idt: String): Event =
        Event.PushIndent(Right(idt))

    }

    def apply(by: Int): URIO[Logger, Unit] =
      execute(event(by))
    def apply(idt: String): URIO[Logger, Unit] =
      execute(event(idt))

  }

  object popIndent {

    val event: Event = Event.PopIndent
    def apply(): URIO[Logger, Unit] =
      execute(event)

  }

  object withIndent {

    def apply[R, E, A](by: Int)(zio: ZIO[R, E, A]): ZIO[Logger with R, E, A] =
      indent(by).bracket(_ => popIndent())(_ => zio)
    def apply[R, E, A](idt: String)(zio: ZIO[R, E, A]): ZIO[Logger with R, E, A] =
      indent(idt).bracket(_ => popIndent())(_ => zio)

  }

  object break {

    def event(
        `type`: Event.Break.Type = Event.Break.Type.Normal,
        print: Event.Break.Print = Event.Break.Print.TagAndIndent,
    ): Event =
      Event.Break(`type`, print)
    def apply(
        `type`: Event.Break.Type = Event.Break.Type.Normal,
        print: Event.Break.Print = Event.Break.Print.TagAndIndent,
    ): URIO[Logger, Unit] =
      execute(Event.Break(`type`, print))

    sealed abstract class Brk(`type`: Event.Break.Type) {

      def event(print: Event.Break.Print = Event.Break.Print.TagAndIndent): Event =
        Event.Break(`type`, print)
      def apply(print: Event.Break.Print = Event.Break.Print.TagAndIndent): URIO[Logger, Unit] =
        execute(event(print))

    }

    object open extends Brk(Event.Break.Type.Open)
    object normal extends Brk(Event.Break.Type.Normal)
    object close extends Brk(Event.Break.Type.Close)

  }

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

    trait Ops {
      def print(message: Any): UIO[Unit]
      def println(message: Any): UIO[Unit]
      def log(message: Any): UIO[Unit]
    }

    // ---  ---

    def stdOut(
        logTolerance: LogLevel with LogLevel.Tolerance,
        initialQueuedBreak: Option[Event.Break] = None,
    ): UIO[Source] =
      for {
        queuedBreak <- Ref.make(initialQueuedBreak)
        ops = new Ops {
          def print(message: Any): UIO[Unit] = ZIO { scala.Console.print(message) }.orDie
          def println(message: Any): UIO[Unit] = ZIO { scala.Console.println(message) }.orDie
          def log(message: Any): UIO[Unit] = ZIO { scala.Console.println(message) }.orDie
        }
      } yield new Source("StdOut", logTolerance, queuedBreak) {
        override type Src = ops.type
        override val acquire: UIO[Src] = UIO(ops)
        override def release(src: Src): UIO[Unit] = UIO.unit
      }

    def file(
        file: File,
        logTolerance: LogLevel with LogLevel.Tolerance,
        initialQueuedBreak: Option[Event.Break] = None,
    ): UIO[Source] = {
      final class BufferedWriterOps(val bw: BufferedWriter) extends Ops {
        override def print(message: Any): UIO[Unit] =
          ZIO.attempt(bw.write(message.toString)).orDie
        override def println(message: Any): UIO[Unit] =
          (ZIO.attempt(bw.write(message.toString)) *> ZIO.attempt(bw.write('\n'))).orDie
        override def log(message: Any): UIO[Unit] =
          (ZIO.attempt(bw.write(message.toString)) *> ZIO.attempt(bw.write('\n'))).orDie
      }

      for {
        _ <- file.createIfDNE.orDie
        queuedBreak <- Ref.make(initialQueuedBreak)
      } yield new Source(file.toString, logTolerance, queuedBreak) {
        override type Src = BufferedWriterOps
        override protected val acquire: UIO[Src] = file.bufferedWriter(StandardOpenOption.APPEND).map(BufferedWriterOps(_)).orDie
        override protected def release(src: Src): UIO[Unit] = ZIO.attempt(src.bw.close()).orDie
      }
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
    final case class Break(`type`: Break.Type, print: Break.Print) extends Event
    object Break {
      enum Type { case Open, Normal, Close }
      enum Print { case Nothing, Tag, TagAndIndent }
    }

    final case class Output(`type`: Output.Type, message: Any) extends Event
    object Output {
      enum Type { case Print, Println, Log }
    }

    final case class PushIndent(indent: Either[Int, String]) extends Event
    case object PopIndent extends Event

    final case class RequireFlags(flags: Set[String], event: () => Event) extends Event {
      override def toString: String = s"RequireFlags($flags,${event()})"
    }
    final case class RequireLogLevel(logLevel: LogLevel, event: () => Event) extends Event {
      override def toString: String = s"RequireLogLevel($logLevel,${event()})"
    }

    def apply(event: Event): Event =
      event

    def apply(event0: Event, event1: Event, eventN: Event*): Event =
      Compound(event0 :: event1 :: eventN.toList)

    val empty: Event =
      Compound(Nil)

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