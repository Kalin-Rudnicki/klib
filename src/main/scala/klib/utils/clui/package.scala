package klib.utils

import klib.Implicits._
import klib.fp.types._

import scala.annotation.tailrec

package object clui {

  def clui[V](
      reader: Reader,
      initialState: State.Input[V] = State.Empty,
  )(
      handle: (Logger, State.Input[V], List[String]) => IO[State.Output[V]],
  ): Executable =
    (logger, _) => { // TODO (KR) : Do something with args as well?
      @tailrec
      def loop(s: State.Input[V]): IO[Unit] = {
        val body =
          for {
            args <- reader.read
            res <- handle(logger, s, args)
          } yield res

        body.runSync match {
          case Right(r) =>
            r match {
              case input: State.Input[V] =>
                loop(input)
              case State.Exit =>
                ().pure[IO]
              case State.Unknown =>
                for {
                  _ <- logger() { src =>
                    src.error("Unknown Command")
                  }
                  res <- loop(s)
                } yield res
            }
          case Left(a) =>
            IO(throw a)
        }
      }

      loop(initialState).wrap
    }

}
