package klib.utils

import scala.annotation.tailrec

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._

trait Executable {

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

    val (loggerArgs, programArgs) = split_--(args.toList, Nil)

    val logger = new Executable.LoggerConf(loggerArgs).logger
    val result = execute(logger, programArgs)

    val (res, warnings, errors) = result.run.toTuple

    logger() { src =>
      def show(name: String, level: Logger.LogLevel, throwables: List[Throwable]): Unit =
        if (throwables.nonEmpty) {
          src.log(level, s"=====| $name${if (throwables.size == 1) "" else "s"} [${throwables.size}] |=====")
          throwables.foreach { throwable =>
            src.log(level, throwable.getMessage)
            throwable.getStackTrace.foreach(src.log(Logger.LogLevel.Debug, _))
          }
        }

      show("Warning", Logger.LogLevel.Warning, warnings)
      show("Error", Logger.LogLevel.Fatal, errors)

      val resMsg =
        if (res.isEmpty)
          "<Failure>".toColorString.red
        else
          "<Success>".toColorString.green
      src.important(resMsg)
    }
  }

  def execute(logger: Logger, args: List[String]): ??[Unit]

}

object Executable {

  // =====| Helpers |=====

  def fromSubCommands(subCommands: (String, Executable)*): Executable = {
    val commandMap: Map[String, Executable] = subCommands.toMap
    val opts: List[String] = commandMap.toList.map(_._1).sorted

    def makeError(message: String): ??[Nothing] =
      (Dead(Message(s"$message. Options: ${opts.mkString(", ")}") :: Nil): ?[Nothing]).wrap[IO]

    { (logger, args) =>
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

  trait ExecFromConf extends Executable {

    type Conf

    def buildConf(args: Seq[String]): Conf

    def run(logger: Logger, conf: Conf): ??[Unit]

    override def execute(logger: Logger, args: List[String]): ??[Unit] =
      run(logger, buildConf(args))

  }

  // =====| Logger |=====

  private implicit val logLevelConverter: ValueConverter[Logger.LogLevel] =
    new ValueConverter[Logger.LogLevel] {
      override val argType: ArgType.V = ArgType.SINGLE

      private val logLevelMap =
        Logger.LogLevel.All
          .flatMap(ll =>
            (ll.priority.toString -> ll) :: (ll.name.toUpperCase -> ll) :: (ll.displayName.toUpperCase -> ll) :: Nil,
          )
          .toMap

      override def parse(s: List[(String, List[String])]): scala.Either[String, Option[Logger.LogLevel]] =
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

  final class LoggerConf(args: Seq[String]) extends ScallopConf(args) {
    banner("[Note] Commands are expected in format of: [LOGGER_OPTS...] '--' [PROGRAM_OPTS...]")
    banner("       If no '--' is provided, all args are considered to be [PROGRAM_OPTS...]")

    val logTolerance: ScallopOption[Logger.LogLevel] = opt[Logger.LogLevel](default = Logger.LogLevel.Info.someOpt)
    val flags: ScallopOption[Set[String]] = opt[List[String]](default = Nil.someOpt).map(_.toSet)
    val idtStr: ScallopOption[String] = opt[String](default = "    ".someOpt)

    verify()

    def logger: Logger =
      Logger(
        logTolerance = logTolerance(),
        flags = flags(),
        indentStr = idtStr(),
      )

  }

}
