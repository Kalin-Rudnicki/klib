package klib.fp.types

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.utils.ado

final class ??[+T](val wrapped: IO[?[T]]) {

  def runSync: ?[T] =
    wrapped.execute().to_?.flatten

  def bracket[T2](`try`: T => ??[T2])(`finally`: T => ??[Unit]): ??[T2] =
    this
      .flatMap { self =>
        ado[??]
          .join(
            `try`(self),
            `finally`(self),
          )
      }
      .map(_._1)

  def runAndDumpMessages(): Unit = {
    def dump[M](label: String, throwables: List[M]): Unit =
      if (throwables.nonEmpty) {
        Console.err.println(s"=====| $label${(throwables.size == 1) ? "" | "s"} (${throwables.size}) ---")
        throwables.foreach(Console.err.println)
        Console.err.println()
      }

    runSync match {
      case Alive(_) =>
      case Dead(errors) =>
        dump("Error", errors)
    }
  }

}

object ?? {

  def apply[R](r: => R): ??[R] =
    new ??(r.pure[?].pure[IO])

  def dead(throwables: Throwable*): ??[Nothing] =
    new ??(?.dead(throwables: _*).pure[IO])

  implicit val `??monad`: Monad[??] =
    new Monad[??] {
      override def map[A, B](t: ??[A], f: A => B): ??[B] =
        new ??(t.wrapped.map(_.map(f)))

      override def apply[A, B](t: ??[A], f: ??[A => B]): ??[B] =
        new ??(
          IO.wrapTry {
            val evaledT = t.wrapped.execute()
            val evaledF = f.wrapped.execute()

            for {
              t <- evaledT
              f <- evaledF
            } yield t.apply(f)
          },
        )

      override def pure[A](a: => A): ??[A] =
        ??(a)

      override def flatten[A](t: ??[??[A]]): ??[A] =
        new ??(
          IO {
            for {
              _1 <- t.wrapped.execute().to_?.flatten
              _2 <- _1.wrapped.execute().to_?.flatten
            } yield _2
          },
        )

    }

  implicit val `??traverseList`: Traverse[List, ??] =
    new Traverse[List, ??] {

      override def traverse[T](t: List[??[T]]): ??[List[T]] =
        new ??(ErrorAccumulator.errorAccumulatorTraverseList.traverse(t.map(_.runSync)).pure[IO])

    }

}
