package klib.utils

import scala.annotation.tailrec

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._
import klib.utils.Logger.{helpers => L}, L.Implicits._

final case class Executable(execute: (Logger, List[String]) => IO[Unit]) {

  def apply(args: Array[String]): IO[Unit] = {
    @tailrec
    def split_--(queue: List[String], stack: List[String]): (List[String], List[String]) =
      queue match {
        case "--" :: tail =>
          (stack.reverse, tail)
        case head :: tail =>
          split_--(tail, head :: stack)
        case Nil =>
          /*
              TODO (KR) : Decide how this should be handled
                        : ["A", "B"] => ([], ["A", "B"])
                        : ["A", "B"] => (["A", "B"], [])
                        : Update banner in LoggerConf if this changes
           */
          (Nil, stack.reverse)
      }

    def throwablesEvent(
        label: String,
        messageLevel: Logger.LogLevel,
        throwables: List[Throwable],
    ): Logger.Event =
      if (throwables.nonEmpty)
        L(
          L.log(messageLevel, s"=====| $label${(throwables.size == 1) ? "" | "s"} [${throwables.size}] |====="),
          L.indented(
            throwables.map(L.log.throwable(_, messageLevel)),
          ),
        )
      else
        L()

    def resEvent(res: Maybe[Unit]): Logger.Event =
      L.log.always(
        res.isEmpty ?
          "<Failure>".toColorString.red |
          "<Success>".toColorString.green,
      )

    val (loggerArgs, programArgs) = split_--(args.toList, Nil)

    Executable.LoggerConf(loggerArgs) match {
      case Right(loggerConf) =>
        val logger = loggerConf.logger
        for {
          _ <- loggerConf.clear().maybe(logger(L(L.ansi.cursorPos(1, 1), L.ansi.clearScreen()))).traverse
          res <- execute(logger, programArgs).runSync.pure[IO]
          _ <- logger(
            res match {
              case Alive(r) =>
                resEvent(r.some)
              case Dead(errors) =>
                L(
                  throwablesEvent("Error", Logger.LogLevel.Fatal, errors),
                  resEvent(None),
                )
            },
          )
        } yield ()
      case Left(logEvent) =>
        Logger(Logger.LogLevel.Info)(logEvent)
    }
  }

}
object Executable {

  // =====| Helpers |=====

  def fromSubCommands(subCommands: (String, Executable)*): Executable = {
    val commandMap: Map[String, Executable] = subCommands.toMap
    val opts: List[String] = commandMap.toList.map(_._1).sorted

    def makeError(message: String): IO[Nothing] =
      IO.error(Message(s"$message. Options: ${opts.mkString(", ")}"))

    Executable { (logger, args) =>
      args match {
        case cmd :: args =>
          commandMap.get(cmd).toMaybe match {
            case Some(command) =>
              command.execute(logger, args)
            case None =>
              makeError(s"Invalid sub-command ($cmd)")
          }
        case Nil =>
          makeError("Missing sub-command")
      }
    }
  }

  final case class LogEventError(logEvent: Logger.Event) extends RuntimeException

  abstract class Conf(args: Seq[String]) extends ScallopConf(args) {

    def helpString: String =
      List(
        this.builder.vers.toMaybe,
        this.builder.bann.toMaybe,
        this.builder.help.some,
        this.builder.foot.toMaybe,
      ).flatten.mkString("\n")

    override def onError(e: Throwable): Unit = {
      import org.rogach.scallop.exceptions._
      e match {
        case Help(_) => throw LogEventError(L.log.info(this.helpString))
        case Version => throw LogEventError(L.log.info(this.builder.vers.toMaybe.cata(identity, "No version")))
        case e       => throw LogEventError(L.log.throwable(e))
      }
    }

  }
  abstract class ConfBuilder[C <: Conf](build: Seq[String] => C) {
    def apply(args: Seq[String]): Logger.Event \/ C = {
      import scala.util._
      Try {
        build(args)
      } match {
        case Success(conf) =>
          conf.right
        case Failure(exception) =>
          exception match {
            case LogEventError(logEvent) => logEvent.left
            case _                       => L.log.throwable(exception).left
          }
      }
    }
  }

  def fromConf[C <: Conf](builder: ConfBuilder[C])(run: (Logger, C) => IO[Unit]): Executable =
    Executable { (logger, args) =>
      builder(args) match {
        case Right(conf) =>
          run(logger, conf)
        case Left(logEvent) =>
          logger(logEvent)
      }
    }

  // =====| Logger |=====

  private implicit val logLevelConverter: ValueConverter[Logger.LogLevel with Logger.LogLevel.Tolerance] =
    new ValueConverter[Logger.LogLevel with Logger.LogLevel.Tolerance] {
      override val argType: ArgType.V = ArgType.SINGLE

      private val logLevelMap =
        Logger.LogLevel.AllTolerance
          .flatMap(ll =>
            (ll.priority.toString -> ll) :: (ll.name.toUpperCase -> ll) :: (ll.displayName.toUpperCase -> ll) :: Nil,
          )
          .toMap

      override def parse(
          s: List[(String, List[String])],
      ): scala.Either[String, Option[Logger.LogLevel with Logger.LogLevel.Tolerance]] =
        s match {
          case (_, arg :: Nil) :: Nil =>
            logLevelMap.get(arg.toUpperCase) match {
              case scala.Some(value) =>
                scala.Right(scala.Some(value))
              case scala.None =>
                scala.Left(s"Illegal log-level: $arg")
            }
          case Nil =>
            scala.Right(scala.None)
          case _ =>
            scala.Left("Mal-formatted log-level")
        }

    }

  final class LoggerConf(args: Seq[String]) extends Conf(args) {
    banner {
      """[Note] Commands are expected in format of: [LOGGER_OPTS...] '--' [PROGRAM_OPTS...]
        |       If no '--' is provided, all args are considered to be [PROGRAM_OPTS...]
        |""".stripMargin
    }

    val logTolerance: ScallopOption[Logger.LogLevel with Logger.LogLevel.Tolerance] =
      opt[Logger.LogLevel with Logger.LogLevel.Tolerance](default = Logger.LogLevel.Info.someOpt)
    val flags: ScallopOption[Set[String]] = opt[List[String]](default = Nil.someOpt).map(_.toSet)
    val ignoredPackages: ScallopOption[Set[String]] = opt[List[String]](default = Nil.someOpt).map(_.toSet)
    val idtStr: ScallopOption[String] = opt[String](default = "    ".someOpt)

    val clear: ScallopOption[Boolean] = opt[Boolean]()

    verify()

    def logger: Logger =
      Logger(
        defaultLogTolerance = logTolerance(),
        defaultFlags = flags(),
        defaultIndentStr = idtStr(),
        defaultIgnorePackages = ignoredPackages(),
      )

  }
  object LoggerConf extends ConfBuilder(new LoggerConf(_))

}
