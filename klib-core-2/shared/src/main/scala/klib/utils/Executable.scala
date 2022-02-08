package klib.utils

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.syntax.option.*
import zio.*

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

  // TODO (KR) :

}
