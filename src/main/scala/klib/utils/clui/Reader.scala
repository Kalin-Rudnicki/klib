package klib.utils.clui

import klib.Implicits._
import klib.fp.types._

import scala.annotation.tailrec

final class Reader(lineReader: LineReader) {

  def read: IO[List[String]] = {

    sealed trait ParseState
    object ParseState {
      final case class Quoted(chars: List[Char], escaped: Boolean) extends ParseState
      final case class UnQuoted(chars: List[Char], escaped: Boolean) extends ParseState
      final case class Waiting(escaped: Boolean) extends ParseState
    }

    @tailrec
    def parse(
        parseState: ParseState,
        queue: List[Char],
        stack: List[String],
    ): (List[String], Maybe[ParseState]) =
      queue match {
        case Nil =>
          parseState match {
            case ParseState.Quoted(chars, escaped) =>
              if (escaped)
                (
                  stack.reverse,
                  ParseState.Quoted(chars, false).some,
                )
              else
                (
                  stack.reverse,
                  ParseState.Quoted('\n' :: chars, false).some,
                )
            case ParseState.UnQuoted(chars, escaped) =>
              (
                (chars.reverse.mkString :: stack).reverse,
                escaped.maybe(ParseState.Waiting(false)),
              )
            case ParseState.Waiting(escaped) =>
              (
                stack.reverse,
                escaped.maybe(ParseState.Waiting(false)),
              )
          }
        case c :: tail =>
          parseState match {
            case ParseState.Quoted(chars, escaped) =>
              if (escaped)
                parse(
                  ParseState.Quoted(
                    c match {
                      case '"' | '\\' =>
                        c :: chars
                      case 't' =>
                        '\t' :: chars
                      case 'n' =>
                        '\n' :: chars
                      case _ =>
                        c :: '\\' :: chars
                    },
                    false,
                  ),
                  tail,
                  stack,
                )
              else
                c match {
                  case '"' =>
                    parse(
                      ParseState.Waiting(false),
                      tail,
                      chars.reverse.mkString :: stack,
                    )
                  case '\\' =>
                    parse(
                      ParseState.Quoted(chars, true),
                      tail,
                      stack,
                    )
                  case _ =>
                    parse(
                      ParseState.Quoted(c :: chars, false),
                      tail,
                      stack,
                    )
                }
            case ParseState.UnQuoted(chars, escaped) =>
              if (escaped)
                parse(
                  ParseState.UnQuoted(
                    c match {
                      case '"' | '\\' =>
                        c :: chars
                      case 't' =>
                        '\t' :: chars
                      case 'n' =>
                        '\n' :: chars
                      case _ =>
                        c :: '\\' :: chars
                    },
                    false,
                  ),
                  tail,
                  stack,
                )
              else
                c match {
                  case ' ' | '\t' =>
                    parse(
                      ParseState.Waiting(false),
                      tail,
                      chars.reverse.mkString :: stack,
                    )
                  case _ =>
                    parse(
                      ParseState.UnQuoted(c :: chars, false),
                      tail,
                      stack,
                    )
                }
            case ParseState.Waiting(escaped) =>
              if (escaped)
                parse(
                  ParseState.UnQuoted(
                    c match {
                      case '"' | '\\' =>
                        c :: Nil
                      case 't' =>
                        '\t' :: Nil
                      case 'n' =>
                        '\n' :: Nil
                      case _ =>
                        c :: '\\' :: Nil
                    },
                    false,
                  ),
                  tail,
                  stack,
                )
              else
                c match {
                  case ' ' | '\t' =>
                    parse(
                      ParseState.Waiting(false),
                      tail,
                      stack,
                    )
                  case '"' =>
                    parse(
                      ParseState.Quoted(Nil, false),
                      tail,
                      stack,
                    )
                  case '\\' =>
                    parse(
                      ParseState.Waiting(true),
                      tail,
                      stack,
                    )
                  case _ =>
                    parse(
                      ParseState.UnQuoted(c :: Nil, false),
                      tail,
                      stack,
                    )
                }
          }
      }

    def rec(fromPrev: Maybe[ParseState]): IO[List[String]] =
      for {
        line <- lineReader.readLine(fromPrev.isEmpty)
        (list, ps) = parse(fromPrev.getOrElse(ParseState.Waiting(false)), line.toList, Nil)
        items <- ps match {
          case Some(ps) =>
            rec(ps.some).map(list ::: _)
          case None =>
            list.pure[IO]
        }
      } yield items

    rec(None)
  }

}

object Reader {

  def apply(lineReader: LineReader): Reader =
    new Reader(lineReader)

}
