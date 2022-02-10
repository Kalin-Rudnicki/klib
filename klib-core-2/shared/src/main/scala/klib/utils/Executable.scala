package klib.utils

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.syntax.option.*
import zio.*

import klib.utils.commandLine.parse.*

// TODO (KR) :
object Executable {

  // =====| Type Aliases |=====

  type Env = FileSystem with Logger with RunMode

  // =====| Builders |=====

  // TODO (KR) :

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
  )
  object KLibConf {

    // TODO (KR) : Fix this

    val parser: Parser[KLibConf] = {
      Parser.singleValue[Logger.LogLevel]("log-tolerance").addDescription("Minimum log level").default(Logger.LogLevel.Info) >&>
        Parser.singleValue[String]("flags").many.addDescription("Add flags to logger").default(Nil).map(_.toSet) >&>
        Parser.singleValue[String]("idt-str").addDescription("Indent string for logger").default("    ") >&>
        Parser.toggle("clear").addDescription("Clear the screen before execution").default(false)
    }.map(KLibConf.apply)

  }

  // TODO (KR) :

  def main(args: Array[String]): Unit = {
    println(KLibConf.parser.disallowExtras.helpString(HelpConfig.default(false)))
    println(KLibConf.parser.disallowExtras.helpString(HelpConfig.default(true)))
  }

}
