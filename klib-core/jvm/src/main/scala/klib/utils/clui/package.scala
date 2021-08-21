package klib.utils

import klib.Implicits._
import klib.fp.types._

package object clui {

  def apply[V](
      reader: Reader = Reader(LineReader.stdIn("\n> ", "  ")),
      initialState: State.Input[V] = State.Empty,
  )(
      handle: (Logger, State.Input[V], List[String]) => IO[State.Output[V]],
  ): Executable =
    Executable { (logger, _) => // TODO (KR) : Do something with args as well?
      def loop(s: State.Input[V]): IO[Unit] = {
        for {
          args <- reader.read
          res <- handle(logger, s, args)
          _ <- res match {
            case s2: State.Input[V] =>
              loop(s2)
            case State.Exit =>
              ().pure[IO]
            case State.Unknown =>
              for {
                _ <- logger.log.error("Unknown Command")
                _ <- loop(s)
              } yield ()
          }
        } yield ()
      }

      loop(initialState).wrap
    }

}
