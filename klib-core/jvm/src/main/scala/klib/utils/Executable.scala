package klib.utils

import scala.annotation.tailrec

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._

final case class Executable(
    execute: (Logger, List[String]) => IO[Unit],
    mapLoggerF: Logger => Logger = identity,
) {

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
              NOTE : ["A", "B"] => ([], ["A", "B"])
                   : ["A", "B"] => (["A", "B"], [])
                   : Update banner in LoggerConf if this changes
           */
          (Nil, stack.reverse)
      }

    val (loggerArgs, programArgs) = split_--(args.toList, Nil)

    Executable.LoggerConf(loggerArgs) match {
      case Right(loggerConf) =>
        val logger = mapLoggerF(loggerConf.logger)
        for {
          _ <- loggerConf.clear().maybe(logger.log(L(L.ansi.cursorPos(1, 1), L.ansi.clearScreen()))).traverse
          res <- execute(logger, programArgs).runSync.pure[IO]
          _ <- logger.log(
            res match {
              case Alive(_) =>
                L.log.always("<Success>".toColorString.green) // TODO (KR) : Only include with flag (?)
              case Dead(errors) =>
                L(
                  L.log.fatal(s"=====| Error${(errors.size == 1) ? "" | "s"} [${errors.size}] |====="),
                  errors.map(L.log.throwable(_)),
                  L.log.always("<Failure>".toColorString.red), // TODO (KR) : Only include with flag (?)
                )
            },
          )
        } yield ()
      case Left(logEvent) =>
        Logger(Logger.LogLevel.Info).log(logEvent)
    }
  }

  def mapLogger(f: Logger => Logger): Executable =
    Executable(execute = execute, mapLoggerF = logger => f(mapLoggerF(logger)))

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
      ).flatMap(_.toOption)
        .mkString("\n")

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
          logger.log(logEvent)
      }
    }

  // =====| Logger |=====

  private implicit val logLevelConverter: ValueConverter[Logger.LogLevel with Logger.LogLevel.Tolerance] =
    new ValueConverter[Logger.LogLevel with Logger.LogLevel.Tolerance] {
      override val argType: ArgType.V = ArgType.SINGLE

      private val logLevelMap =
        Logger.LogLevel.AllTolerance
          .flatMap(ll => (ll.priority.toString -> ll) :: (ll.name.toUpperCase -> ll) :: (ll.displayName.toUpperCase -> ll) :: Nil)
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
