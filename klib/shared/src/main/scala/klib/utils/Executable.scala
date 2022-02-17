package klib.utils

import scala.annotation.tailrec

import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import zio.*

import klib.utils.commandLine.parse.*

trait ExecutableApp extends ZIOAppDefault {

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    for {
      zioAppArgs <- ZIO.service[ZIOAppArgs]
      exitCode <- executable.execute(zioAppArgs.getArgs.toList)
      _ <- exit(exitCode)
    } yield ()

  val executable: Executable

}

opaque type Executable = (List[String], List[String]) => ZIO[Executable.Env & ZEnv, NonEmptyList[Message], Unit]
extension (executable: Executable) {

  def execute(subCommands: List[String], programArgs: List[String]): ZIO[Executable.Env & ZEnv, NonEmptyList[Message], Unit] =
    executable(subCommands, programArgs)

  def execute(args: List[String]): URIO[ZEnv, ExitCode] = {
    def defaultLayer: ULayer[Executable.Env] =
      FileSystem.live.mapError(_ => new RuntimeException).orDie ++ // TODO (KR) : ...
        Logger.live(Logger.LogLevel.Info) ++
        ZLayer.succeed(RunMode.User)

    def configToLayer(conf: Executable.KLibConf): ULayer[Executable.Env] =
      FileSystem.live.mapError(_ => new RuntimeException).orDie ++ // TODO (KR) : ...
        Logger.live(conf.logTolerance, defaultIndent = conf.idtStr, flags = conf.flags) ++
        ZLayer.succeed(conf.runMode)

    def parseConf(args: List[String]): (ULayer[Executable.Env], Option[EitherNel[Message, BuiltParser.Result.Help]]) =
      Executable.KLibConf.builtParser.parse(args) match {
        case BuiltParser.Result.Success(result) => (configToLayer(result), None)
        case BuiltParser.Result.Failure(errors) => (defaultLayer, errors.map(_.toMessage).asLeft.some)
        case help: BuiltParser.Result.Help      => (defaultLayer, help.asRight.some)
      }

    def runProgram(
        subCommands: List[String],
        programArgs: List[String],
        other: Option[EitherNel[Message, BuiltParser.Result.Help]],
    ): URIO[ZEnv & Executable.Env, ExitCode] = {
      def dumpErrors(errors: NonEmptyList[Message]): URIO[ZEnv & Executable.Env, ExitCode] =
        for {
          loggerEvents <- ZIO.foreach(errors.toList)(_.toLoggerEvent(Logger.LogLevel.Fatal))
          _ <- Logger.execute.all(loggerEvents)
        } yield ExitCode.failure

      def showHelp(help: BuiltParser.Result.Help): URIO[ZEnv & Executable.Env, ExitCode] =
        Logger.println.info(help.helpString).as(ExitCode.success)

      other match {
        case Some(other) =>
          other match {
            case Left(errors) => dumpErrors(errors)
            case Right(help)  => showHelp(help)
          }
        case None =>
          executable.execute(subCommands, programArgs).either.flatMap {
            case Left(errors) => dumpErrors(errors)
            case Right(value) => ZIO.succeed(ExitCode.success)
          }
      }
    }

    val splitArgs = Executable.SplitArgs.fromArgs(args)
    val (layer, other) = parseConf(splitArgs.configArgs)
    runProgram(splitArgs.subCommands, splitArgs.programArgs, other).provideSomeLayer(layer)
  }

}
object Executable {

  // =====| Type Aliases |=====

  type Env = FileSystem with Logger with RunMode

  // =====| Builders |=====

  final case class Builder1[P] private[Executable] (
      private val parser: BuiltParser[P],
  ) {
    def withLayer[R: Tag](layerF: P => ZLayer[Env & ZEnv, NonEmptyList[Message], R]): Builder2[P, R] =
      Builder2(parser, layerF)
  }

  final case class Builder2[P, R: Tag] private[Executable] (
      private val parser: BuiltParser[P],
      private val layerF: P => ZLayer[Env & ZEnv, NonEmptyList[Message], R],
  ) {
    def withExecute(execute: P => ZIO[R & Env & ZEnv, NonEmptyList[Message], Unit]): Executable = { (subCommands, args) =>
      subCommands match {
        case Nil =>
          parser.parse(args) match {
            case BuiltParser.Result.Success(result) =>
              execute(result).provideSomeLayer(layerF(result))
            case BuiltParser.Result.Failure(errors) =>
              ZIO.fail(errors.map(_.toMessage))
            case BuiltParser.Result.Help(_, helpString) =>
              Logger.println.info(helpString)
          }
        case subCommand :: _ =>
          ZIO.fail(NonEmptyList.one(Message.same(s"Unknown sub-command: $subCommand")))
      }
    }
  }

  def fromParser[P](parser: BuiltParser[P]): Builder1[P] =
    Builder1(parser)

  def fromSubCommands(subs: (String, Executable)*): Executable = { (subCommands, args) =>
    subCommands match {
      case subCommand :: rest =>
        subs.find(_._1 == subCommand) match {
          case Some((_, executable)) => executable(rest, args)
          case None =>
            ZIO.fail(NonEmptyList.one(Message.same(s"Invalid sub-command ($subCommand), options: ${subs.mkString(", ")}")))
        }
      case Nil =>
        ZIO.fail(NonEmptyList.one(Message.same(s"Missing sub-command, options: ${subs.mkString(", ")}")))
    }
  }

  // =====| Helpers |=====

  final case class SplitArgs(
      subCommands: List[String],
      configArgs: List[String],
      programArgs: List[String],
  )
  object SplitArgs {

    def fromArgs(args: List[String]): SplitArgs = {
      @tailrec
      def splitOnDoubleDash(
          queue: List[String],
          stack: List[String],
      ): Option[(List[String], List[String])] =
        queue match {
          case head :: tail =>
            if (head == "--") (stack.reverse, tail).some
            else splitOnDoubleDash(tail, head :: stack)
          case Nil =>
            None
        }

      @tailrec
      def splitOnFirstDash(
          queue: List[String],
          stack: List[String],
      ): (List[String], List[String]) =
        queue match {
          case head :: tail =>
            if (head.startsWith("-")) (stack.reverse, queue)
            else splitOnFirstDash(tail, head :: stack)
          case Nil =>
            (stack.reverse, Nil)
        }

      splitOnDoubleDash(args, Nil) match {
        case Some((beforeDoubleDash, programArgs)) =>
          val (subCommands, configArgs) = splitOnFirstDash(beforeDoubleDash, Nil)
          SplitArgs(
            subCommands = subCommands,
            configArgs = configArgs,
            programArgs = programArgs,
          )
        case None =>
          val (subCommands, programArgs) = splitOnFirstDash(args, Nil)
          SplitArgs(
            subCommands = subCommands,
            configArgs = Nil,
            programArgs = programArgs,
          )
      }
    }

  }

  final case class KLibConf(
      logTolerance: Logger.LogLevel,
      flags: Set[String],
      idtStr: String,
      clearScreen: Boolean,
      runMode: RunMode,
  )
  object KLibConf {

    val parser: Parser[KLibConf] = {
      Parser.singleValue
        .enumValues("log-tolerance", Logger.LogLevel.All, ll => List(ll.name, ll.displayName))
        .addDescription("Minimum log level")
        .default(Logger.LogLevel.Info) >&>
        Parser.singleValue[String]("flags").many.addDescription("Add flags to logger").default(Nil).map(_.toSet) >&>
        Parser.singleValue[String]("idt-str").addDescription("Indent string for logger").default("    ") >&>
        Parser.toggle("clear").addDescription("Clear the screen before execution").default(false) >&>
        Parser.singleValue.enumValues("run-mode", RunMode.values).default(RunMode.User)
    }.map(KLibConf.apply)

    val builtParser: BuiltParser[KLibConf] =
      parser.disallowExtras

  }

}
