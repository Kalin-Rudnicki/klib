package klib.utils

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.syntax.option._
import zio.*

opaque type Executable[-R] = List[String] => ZIO[Executable.Environment[R], NonEmptyList[Message], Unit]
extension [R](executable: Executable[R]) {

  def run: ZIO[R with ZEnv with ZIOAppArgs, Nothing, Unit] = ???

}

object Executable {

  // =====| Type Aliases |=====

  type Environment[R] = R & ZEnv & Env
  type Env = FileSystem with Logger with RunMode

  // =====| Builders |=====

  // TODO (KR) :

  // =====| Helpers |=====

  final case class SplitArgs(
      subCommands: List[String],
      loggerArgs: List[String],
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
          val (subCommands, loggerArgs) = splitOnFirstDash(beforeDoubleDash, Nil)
          SplitArgs(
            subCommands = subCommands,
            loggerArgs = loggerArgs,
            programArgs = programArgs,
          )
        case None =>
          val (subCommands, programArgs) = splitOnFirstDash(args, Nil)
          SplitArgs(
            subCommands = subCommands,
            loggerArgs = Nil,
            programArgs = programArgs,
          )
      }
    }

  }

  // TODO (KR) :

}
