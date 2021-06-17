package klib.fp.types

import java.awt.image.BufferedImage
import java.io.{File, PrintWriter, RandomAccessFile}
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.utils.ado

final class IO[+T] private (val unsafeValueF: () => T) {

  def wrap: ??[T] =
    new WrappedErrorAccumulator(
      IO(unsafeValueF().pure[?]),
    )

  def bracket[T2](`try`: T => IO[T2])(`finally`: T => IO[Unit])(implicit ioMonad: Monad[IO]): IO[T2] =
    this
      .flatMap { self =>
        ado[IO]
          .join(
            `try`(self),
            `finally`(self),
          )
      }
      .map(_._1)

}

object IO {

  def apply[T](value: => T): IO[T] =
    new IO(() => value)

  // TODO (KR) : I dont like this, but the alternative seems to be way more wonky and complex, especially for delayed evaluation
  def error(throwable: Throwable): IO[Nothing] =
    IO { throw throwable }

  def now: IO[Long] =
    System.currentTimeMillis.pure[IO]

  def readFile(path: File): IO[String] =
    IO(Source.fromFile(path)).bracket(_.mkString.pure[IO])(_.close.pure[IO])

  def readFileBytes(path: File): IO[Array[Byte]] =
    IO(new RandomAccessFile(path, "r")).bracket { raf =>
      for {
        len <- raf.length.pure[IO]
        bb = new Array[Byte](len.toInt) // TODO (KR) : Could be an issue...
        _ <- raf.readFully(bb).pure[IO]
      } yield bb
    }(_.close().pure[IO])

  def readImage(path: File): IO[BufferedImage] =
    ImageIO.read(path).pure[IO]

  def writeFile(path: File, contents: String): IO[Unit] =
    new PrintWriter(path).pure[IO].bracket(_.write(contents).pure[IO])(_.close.pure[IO])

  // TODO (KR) : Maybe do this differently?
  implicit val ioMonad: Monad[IO] =
    new Monad[IO] {

      override def map[A, B](t: IO[A], f: A => B): IO[B] =
        IO(f(t.unsafeValueF()))

      override def apply[A, B](t: IO[A], f: IO[A => B]): IO[B] =
        IO {
          // f.unsafeValueF()(t.unsafeValueF()) causes `ado[IO].join` to produce effects in reverse order+
          val evaledT = t.unsafeValueF()
          val evaledF = f.unsafeValueF()
          evaledF(evaledT)
        }

      override def pure[A](a: => A): IO[A] =
        IO(a)

      override def flatten[A](t: IO[IO[A]]): IO[A] =
        IO(t.unsafeValueF().unsafeValueF())

    }

  implicit val ioRunSync: RunSync[IO, Throwable] =
    new RunSync[IO, Throwable] {

      override def runSync[A](t: IO[A]): Throwable \/ A =
        Try(t.unsafeValueF()).to_\/

    }

  implicit val ioTraverseList: Traverse[List, IO] =
    new Traverse[List, IO] {

      override def traverse[T](t: List[IO[T]]): IO[List[T]] =
        IO {
          @tailrec
          def loop(queue: List[IO[T]], stack: List[T]): List[T] =
            queue match {
              case head :: tail =>
                loop(tail, head.unsafeValueF() :: stack)
              case Nil =>
                stack.reverse
            }

          loop(t, Nil)
        }

    }

}
