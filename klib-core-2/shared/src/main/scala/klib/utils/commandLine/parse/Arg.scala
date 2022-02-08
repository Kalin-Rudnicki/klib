package klib.utils.commandLine.parse

import scala.annotation.tailrec
import scala.util.matching.Regex

import cats.syntax.option.*

sealed trait Arg
object Arg {

  // TODO (KR) : Some way to stop this from happening...
  //           : {} (--last-name --first-name First Last)
  //           : { firstName: First } (--last-name Last)
  //           : { firstName: First, lastName: Last } ()d

  // =====| ADT |=====
  final case class ShortParamMulti(name: Char) extends Arg
  final case class ShortParamSingle(name: Char) extends Arg
  final case class ShortParamSingleWithValue(name: Char, value: String) extends Arg
  final case class LongParam(name: String) extends Arg
  final case class LongParamWithValue(name: String, value: String) extends Arg
  final case class Value(value: String) extends Arg

  // =====| ... |=====

  private val EscapedRegex: Regex = "^\\[-](.*)$".r
  private val ShortParamMultiRegex: Regex = "^-([A-Za-z]{2,})$".r
  private val ShortParamSingleRegex: Regex = "^-([A-Za-z])$".r
  private val ShortParamSingleWithValueRegex: Regex = "^-([A-Za-z])=(.*)$".r
  private val LongParamRegex: Regex = "^--([A-Za-z][A-Za-z\\d\\-]*)$".r
  private val LongParamWithValueRegex: Regex = "^--([A-Za-z][A-Za-z\\d\\-]*)=(.*)$".r

  def parse(args: List[String]): List[Arg] = {
    def parseArg(arg: String): List[Arg] =
      arg match {
        case EscapedRegex(value)                         => Value(value) :: Nil
        case ShortParamMultiRegex(names)                 => names.toList.distinct.map(ShortParamMulti.apply)
        case ShortParamSingleRegex(name)                 => ShortParamSingle(name.head) :: Nil
        case ShortParamSingleWithValueRegex(name, value) => ShortParamSingleWithValue(name.head, value) :: Nil
        case LongParamRegex(name)                        => LongParam(name) :: Nil
        case LongParamWithValueRegex(name, value)        => LongParamWithValue(name, value) :: Nil
        case value                                       => Value(value) :: Nil
      }

    args.flatMap(parseArg)
  }

  object find {

    final case class Found[+A](
        before: List[Arg],
        arg: A,
        after: List[Arg],
    ) {

      def map[A2](f: A => A2): Found[A2] =
        Found(before, f(arg), after)

      def remaining: List[Arg] =
        before ::: after

    }

    private def findGeneric[A](f: PartialFunction[Arg, A])(args: List[Arg]): Option[Found[A]] = {
      val liftedF: Arg => Option[A] = f.lift

      @tailrec
      def loop(
          queue: List[Arg],
          stack: List[Arg],
      ): Option[Found[A]] =
        queue match {
          case head :: tail =>
            liftedF(head) match {
              case Some(a) => Found(stack.reverse, a, tail).some
              case None    => loop(tail, head :: stack)
            }
          case Nil => None
        }

      loop(args, Nil)
    }

    object basic {

      def shortParamMulti(name: Char)(args: List[Arg]): Option[Found[ShortParamMulti]] =
        findGeneric { case p: ShortParamMulti if p.name == name => p }(args)
      def shortParamSingle(name: Char)(args: List[Arg]): Option[Found[ShortParamSingle]] =
        findGeneric { case p: ShortParamSingle if p.name == name => p }(args)
      def shortParamSingleWithValue(name: Char)(args: List[Arg]): Option[Found[ShortParamSingleWithValue]] =
        findGeneric { case p: ShortParamSingleWithValue if p.name == name => p }(args)

      def longParam(name: String)(args: List[Arg]): Option[Found[LongParam]] =
        findGeneric { case p: LongParam if p.name == name => p }(args)
      def longParamWithValue(name: String)(args: List[Arg]): Option[Found[LongParamWithValue]] =
        findGeneric { case p: LongParamWithValue if p.name == name => p }(args)

    }

    object combined {

      private def firstArgIsValue(args: List[Arg]): Option[(Arg.Value, List[Arg])] =
        args match {
          case (value: Arg.Value) :: remaining => (value, remaining).some
          case _                               => None
        }
      private def withFirstArgIsValue[A](find: List[Arg] => Option[Found[A]])(args: List[Arg]): Option[Found[(A, Value)]] =
        for {
          found <- find(args)
          (value, after) <- firstArgIsValue(found.after)
        } yield Found(found.before, (found.arg, value), after)

      def shortParamWithValue(name: Char)(args: List[Arg]): Option[Found[String]] =
        withFirstArgIsValue(basic.shortParamSingle(name))(args).map(_.map(_._2.value)) orElse
          basic.shortParamSingleWithValue(name)(args).map(_.map(_.value))

      def longParamWithValue(name: String)(args: List[Arg]): Option[Found[String]] =
        withFirstArgIsValue(basic.longParam(name))(args).map(_.map(_._2.value)) orElse
          basic.longParamWithValue(name)(args).map(_.map(_.value))

    }

  }

}
