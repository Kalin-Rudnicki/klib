package klib.fp.types

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.utils.ado

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

final class IO[+T] private (t: => T) {

  private def value: T = t

  def wrap: ??[T] =
    new WrappedErrorAccumulator(
      IO(t.pure[?]),
    )

  def bracket[T2](`try`: T => IO[T2])(`finally`: T => IO[Unit])(implicit ioMonad: Monad[IO]): IO[T2] =
    ado[IO]
      .join(
        ioMonad.flatMap(this, `try`),
        ioMonad.flatMap(this, `finally`),
      )
      .map(_._1)

}

object IO {

  def apply[T](value: => T): IO[T] =
    new IO(value)

  def now: IO[Long] =
    System.currentTimeMillis.pure[IO]

  def readFile(path: File): IO[String] =
    IO(Source.fromFile(path)).bracket(_.mkString.pure[IO])(_.close.pure[IO])

  def readImage(path: File): IO[BufferedImage] =
    ImageIO.read(path).pure[IO]

  // TODO (KR) : Maybe do this differently?
  implicit val ioMonad: Monad[IO] =
    new Monad[IO] {

      override def map[A, B](t: IO[A], f: A => B): IO[B] =
        IO(f(t.value))

      override def apply[A, B](t: IO[A], f: IO[A => B]): IO[B] =
        IO {
          // f.value(t.value) causes `ado[IO].join` to produce effects in reverse order+
          val evaledT = t.value
          val evaledF = f.value
          evaledF(evaledT)
        }

      override def pure[A](a: => A): IO[A] =
        IO(a)

      override def flatten[A](t: IO[IO[A]]): IO[A] =
        IO(t.value.value)

    }

  implicit val ioRunSync: RunSync[IO, Throwable] =
    new RunSync[IO, Throwable] {

      override def runSync[A](t: IO[A]): Throwable \/ A =
        Try(t.value).to_\/

    }

  implicit val ioTraverseList: Traverse[List, IO] =
    new Traverse[List, IO] {

      override def traverse[T](t: List[IO[T]]): IO[List[T]] =
        IO {
          @tailrec
          def loop(queue: List[IO[T]], stack: List[T]): List[T] =
            queue match {
              case head :: tail =>
                loop(tail, head.value :: stack)
              case Nil =>
                stack.reverse
            }

          loop(t, Nil)
        }

    }

}
