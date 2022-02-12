package klib.utils.commandLine.parse

import scala.annotation.tailrec
import scala.util.matching.Regex

import cats.syntax.option.*
import cats.syntax.either.*

type IndexedArgs = List[Indexed[Arg]]
object IndexedArgs {

  val ordering: Ordering[Indexed[Arg]] =
    Ordering.by[Indexed[Arg], Int](_.index).orElseBy {
      case Indexed(Arg.ShortParamMulti(_, i), _) => i
      case _                                     => 0
    }

}

sealed trait Arg
object Arg {

  // TODO (KR) : Some way to stop this from happening...
  //           : {} (--last-name --first-name First Last)
  //           : { firstName: First } (--last-name Last)
  //           : { firstName: First, lastName: Last } ()d

  // =====| ADT |=====

  final case class ShortParamMulti(name: Char, subIdx: Int) extends Arg
  final case class ShortParamSingle(name: Char) extends Arg
  final case class ShortParamSingleWithValue(name: Char, value: String) extends Arg
  final case class LongParam(name: String) extends Arg
  final case class LongParamWithValue(name: String, value: String) extends Arg
  final case class Value(value: String) extends Arg

  // =====| Remaining Args |=====

  def remainingInBoth(remainingArgs1: IndexedArgs, remainingArgs2: IndexedArgs): IndexedArgs =
    (remainingArgs1.toSet & remainingArgs2.toSet).toList.sorted(IndexedArgs.ordering)

  // =====| Parse / Find |=====

  private val EscapedRegex: Regex = "^\\[-](.*)$".r
  private val ShortParamMultiRegex: Regex = "^-([A-Za-z]{2,})$".r
  private val ShortParamSingleRegex: Regex = "^-([A-Za-z])$".r
  private val ShortParamSingleWithValueRegex: Regex = "^-([A-Za-z])=(.*)$".r
  private val LongParamRegex: Regex = "^--([A-Za-z][A-Za-z\\d\\-]*)$".r
  private val LongParamWithValueRegex: Regex = "^--([A-Za-z][A-Za-z\\d\\-]*)=(.*)$".r

  def parse(args: List[String]): IndexedArgs = {
    def parseArg(arg: String): List[Arg] =
      arg match {
        case EscapedRegex(value)                         => Value(value) :: Nil
        case ShortParamMultiRegex(names)                 => names.toList.zipWithIndex.map(ShortParamMulti.apply)
        case ShortParamSingleRegex(name)                 => ShortParamSingle(name.head) :: Nil
        case ShortParamSingleWithValueRegex(name, value) => ShortParamSingleWithValue(name.head, value) :: Nil
        case LongParamRegex(name)                        => LongParam(name) :: Nil
        case LongParamWithValueRegex(name, value)        => LongParamWithValue(name, value) :: Nil
        case value                                       => Value(value) :: Nil
      }

    Indexed.list(args).flatMap(_.mapToList(parseArg))
  }

  object find {

    final case class Found[+A](
        before: IndexedArgs,
        arg: A,
        after: IndexedArgs,
    ) {
      inline def map[A2](f: A => A2): Found[A2] = Found(before, f(arg), after)
      inline def remaining: IndexedArgs = before ::: after
    }

    opaque type FindFunction[+F] = IndexedArgs => Option[Found[F]]
    extension [F](f: FindFunction[F]) {

      def attemptToFind(args: IndexedArgs): Option[Found[F]] = f(args)

      def map[F2](mapF: F => F2): FindFunction[F2] =
        f(_).map(_.map(mapF))

      def ||(f2: FindFunction[F]): FindFunction[F] =
        args => f(args) orElse f2(args)

    }
    object FindFunction {

      def fromParam(param: Param.LongWithValue): FindFunction[String] =
        find(param.name).singleValue
      def fromParam(param: Param.ShortWithValue): FindFunction[String] =
        find(param.name).singleValue
      def fromParam(param: Param.LongToggle): FindFunction[Boolean] =
        find(param.trueName).constValue(true) || find(param.falseName).constValue(false)
      def fromParam(param: Param.ShortToggle): FindFunction[Boolean] =
        find(param.trueName).constValue(true) || find(param.falseName).constValue(false)

      def first[F](fs: FindFunction[F]*): FindFunction[F] = first(fs.toList)
      def first[F](fs: List[FindFunction[F]]): FindFunction[F] =
        fs.foldRight[FindFunction[F]](_ => None) { (f, acc) => f || acc }

    }

    final case class Builder[+N](private val findF: FindFunction[Either[(N, Boolean), (N, String)]]) {

      def noValues: FindFunction[N] =
        findF(_).flatMap { case Found(before, found, after) =>
          found match {
            case Left((name, _)) => Found(before, name, after).some
            case Right(_)        => None
          }
        }

      inline def constValue[V](v: => V): FindFunction[V] =
        noValues(_).map(_.map(_ => v))

      def singleValueWithName: FindFunction[(N, String)] =
        findF(_).flatMap { case Found(before, found, after) =>
          found match {
            case Left((name, canHaveValues)) =>
              if (canHaveValues)
                after match {
                  case Indexed(Arg.Value(value), _) :: afterValue => Found(before, (name, value), afterValue).some
                  case _                                          => None
                }
              else None
            case Right(pair) => Found(before, pair, after).some
          }
        }

      inline def singleValue: FindFunction[String] =
        singleValueWithName(_).map(_.map(_._2))

    }

    // =====|  |=====

    private def findGeneric[N](f: PartialFunction[Arg, N]): FindFunction[N] = {
      val liftedF: Indexed[Arg] => Option[N] = iArg => f.lift(iArg.value)

      @tailrec
      def loop(
          queue: IndexedArgs,
          stack: IndexedArgs,
      ): Option[Found[N]] =
        queue match {
          case head :: tail =>
            liftedF(head) match {
              case Some(a) => Found(stack.reverse, a, tail).some
              case None    => loop(tail, head :: stack)
            }
          case Nil => None
        }

      loop(_, Nil)
    }

    def find(name: Char): Builder[Char] =
      Builder {
        findGeneric {
          case p: ShortParamMulti if p.name == name           => (p.name, false).asLeft
          case p: ShortParamSingle if p.name == name          => (p.name, true).asLeft
          case p: ShortParamSingleWithValue if p.name == name => (p.name, p.value).asRight
        }
      }

    def find(name: String): Builder[String] =
      Builder {
        findGeneric {
          case p: LongParam if p.name == name          => (p.name, true).asLeft
          case p: LongParamWithValue if p.name == name => (p.name, p.value).asRight
        }
      }

    inline def apply(name: Char): Builder[Char] = find(name)
    inline def apply(name: String): Builder[String] = find(name)

  }

}
