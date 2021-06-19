package klib.fp.types

import java.awt.image.BufferedImage
import java.io.{File, PrintWriter, RandomAccessFile}
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Try}
import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.utils.ado

final class IO[+T](val execute: () => Try[T]) {

  def runSync: Throwable \/ T =
    execute().to_\/

  def bracket[T2](`try`: T => IO[T2])(`finally`: T => IO[Unit]): IO[T2] =
    this
      .flatMap { self =>
        ado[IO]
          .join(
            `try`(self),
            `finally`(self),
          )
      }
      .map(_._1)

  def to_?? : ??[T] =
    new ??(this.map(_.pure[?]))

}
object IO {

  // =====|  |=====

  @inline
  def apply[T](t: => T): IO[T] =
    new IO(() => Try(t))

  @inline
  def wrapTry[T](`try`: => Try[T]): IO[T] =
    new IO(() => `try`)

  // =====|  |=====

  def error(throwable: Throwable): IO[Nothing] =
    IO.wrapTry(Failure(throwable))

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

  // =====|  |=====

  implicit val ioMonad: Monad[IO] =
    new Monad[IO] {

      override def map[A, B](t: IO[A], f: A => B): IO[B] =
        IO.wrapTry(t.execute().map(f))

      override def apply[A, B](t: IO[A], f: IO[A => B]): IO[B] = {
        IO.wrapTry {
          val evaledT = t.execute()
          val evaledF = f.execute()

          for {
            t <- evaledT
            f <- evaledF
          } yield f(t)
        }
      }

      override def pure[A](a: => A): IO[A] =
        IO(a)

      override def flatten[A](t: IO[IO[A]]): IO[A] =
        IO.wrapTry(t.execute().flatMap(_.execute()))

    }

  implicit val ioTraverseList: Traverse[List, IO] =
    new Traverse[List, IO] {

      override def traverse[T](t: List[IO[T]]): IO[List[T]] = {
        @tailrec
        def loop(queue: List[IO[T]], stack: Try[List[T]]): Try[List[T]] =
          queue match {
            case head :: tail =>
              val tT = head.execute()
              loop(
                tail,
                for {
                  t <- tT
                  s <- stack
                } yield t :: s,
              )
            case Nil =>
              stack.map(_.reverse)
          }

        IO.wrapTry(loop(t, Try(Nil)))
      }

    }

}
