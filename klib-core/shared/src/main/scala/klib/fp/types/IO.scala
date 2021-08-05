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
import klib.utils.Logger
import klib.utils.Logger.{helpers => L}

final class IO[+T](val execute: () => ?[T]) {

  def toAsyncIO: AsyncIO[T] =
    AsyncIO.wrapIO(this)

  def runSync: ?[T] =
    execute()

  private def runSyncOr[R](logger: Maybe[Logger])(onSuccess: T => R)(onFail: => R): R =
    runSync match {
      case Alive(r) =>
        onSuccess(r)
      case Dead(runSyncErrors) =>
        // TODO (KR) : Do something with `cause`
        def writeToConsoleErr(errors: List[Throwable]): Unit =
          errors.foreach { error =>
            Console.err.println(s"> ${error.getMessage}")
            error.getStackTrace.foreach { ste =>
              Console.err.println(s"    $ste")
            }
          }

        def attemptWriteToLogger(logger: Logger): Unit =
          logger(L(runSyncErrors.map(L.log.throwable(_)))).runSync match {
            case Alive(_) =>
            case Dead(logErrors) =>
              writeToConsoleErr(runSyncErrors ::: logErrors)
          }

        logger match {
          case Some(logger) =>
            attemptWriteToLogger(logger)
          case None =>
            writeToConsoleErr(runSyncErrors)
        }
        onFail
    }

  def runSyncOrDump(logger: Maybe[Logger]): Unit =
    runSyncOr[Unit](logger) { _ => () } { () }

  def runSyncOrThrow(logger: Maybe[Logger]): T =
    runSyncOr[T](logger) { identity } { throw new RuntimeException("runSync did not succeed") }

  def runSyncOrExit(logger: Maybe[Logger]): T =
    runSyncOr[T](logger) { identity } { System.exit(1).asInstanceOf[Nothing] }

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

  def timed(withTime: Long => IO[Unit]): IO[T] =
    for {
      start <- IO.now
      res <- this
      end <- IO.now
      _ <- withTime(end - start)
    } yield res

}
object IO {

  // =====|  |=====

  @inline
  def apply[T](t: => T): IO[T] =
    wrapTry { Try(t) }

  @inline
  def wrapTry[T](`try`: => Try[T]): IO[T] =
    wrapEffect { `try`.to_? }

  @inline
  def wrapEffect[T](effect: => ?[T]): IO[T] =
    new IO(() => effect)

  // =====|  |=====

  def error(error0: Throwable, errorN: Throwable*): IO[Nothing] =
    IO.wrapEffect { Dead(error0 :: errorN.toList) }

  def now: IO[Long] =
    System.currentTimeMillis.pure[IO]

  def readFile(path: File): IO[String] =
    IO(Source.fromFile(path)).bracket(_.mkString.pure[IO])(_.close.pure[IO])

  def readFileBytes(path: File): IO[Array[Byte]] =
    IO(new RandomAccessFile(path, "r")).bracket { raf =>
      for {
        len <- raf.length.pure[IO]
        bb = new Array[Byte](len.toInt) // WARNING (KR) : Could be an issue...
        _ <- raf.readFully(bb).pure[IO]
      } yield bb
    }(_.close().pure[IO])

  def readImage(path: File): IO[BufferedImage] =
    ImageIO.read(path).pure[IO]

  def writeFile(path: File, contents: String): IO[Unit] =
    new PrintWriter(path).pure[IO].bracket(_.write(contents).pure[IO])(_.close().pure[IO])

  def writeFileBytes(path: File, bytes: Array[Byte]): IO[Unit] =
    IO(new RandomAccessFile(path, "rw")).bracket { _.write(bytes).pure[IO] }(_.close().pure[IO])

  // =====|  |=====

  implicit val ioMonad: Monad[IO] =
    new Monad[IO] {

      override def map[A, B](t: IO[A], f: A => B): IO[B] =
        IO.wrapEffect { t.execute().map(a => Try(f(a)).to_?).flatten }

      override def apply[A, B](t: IO[A], f: IO[A => B]): IO[B] = {
        IO.wrapEffect {
          val evaledT = t.execute()
          val evaledF = f.execute()

          evaledT.apply(evaledF)
        }
      }

      override def pure[A](a: => A): IO[A] =
        IO(a)

      override def flatMap[A, B](t: IO[A], f: A => IO[B]): IO[B] =
        IO.wrapEffect { t.execute().flatMap(f(_).execute()) }

    }

  implicit val ioTraverseList: Traverse[List, IO] =
    new Traverse[List, IO] {

      override def traverse[T](t: List[IO[T]]): IO[List[T]] =
        IO.wrapEffect { t.map(_.runSync).traverse }

    }

}
