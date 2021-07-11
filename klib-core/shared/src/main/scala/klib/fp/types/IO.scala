package klib.fp.types

import java.awt.image.BufferedImage
import java.io.{File, PrintWriter, RandomAccessFile}
import javax.imageio.ImageIO

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

import klib.extensions.{given, _}
import klib.fp.typeclass._
import klib.fp.types.instances.{given, _}
import klib.fp.utils.ado
import klib.utils.Logger
import klib.utils.Logger.{helpers => L}

final class IO[+T](val execute: () => ??[T]) {

  def runSync: ??[T] =
    execute()

  private def runSyncOr[E](logger: Maybe[Logger])(success: T => E)(or: => E): E =
    this.runSync match {
      case Alive(r) =>
        success(r)
      case Dead(runSyncErrors) =>
        def logToConsoleError(errors: List[Throwable]): Unit = {
          // TODO (KR) : Also log `cause`, if applicable
          errors.foreach { error =>
            Console.err.println(s"> ${error.getMessage}")
            error.getStackTrace.foreach { ste =>
              Console.err.println(s"    $ste")
            }
          }
        }
        def attemptLogToLogger(logger: Logger): Unit = {
          logger(L(runSyncErrors.map(L.log.throwable(_)))).runSync match {
            case Alive(_) =>
            case Dead(loggerErrors) =>
              logToConsoleError(runSyncErrors ::: loggerErrors)
          }
        }

        logger match {
          case Some(logger) =>
            attemptLogToLogger(logger)
          case None =>
            logToConsoleError(runSyncErrors)
        }
        or
    }

  def runSyncOrDump(logger: Maybe[Logger]): Unit =
    runSyncOr[Unit](logger) { _ => () } { () }

  def runSyncOrThrow(logger: Maybe[Logger]): T =
    runSyncOr[T](logger) { identity } { throw new RuntimeException("Unable to successfully run IO") }

  def runSyncOrQuit(logger: Maybe[Logger]): T =
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

}
object IO {

  // =====|  |=====

  @inline
  def apply[T](t: => T): IO[T] =
    wrapTry(Try(t))

  @inline
  def wrapTry[T](`try`: => Try[T]): IO[T] =
    wrapEffect(`try`.to_??)

  @inline
  def wrapEffect[T](effect: => ??[T]): IO[T] =
    new IO(() => effect)

  // =====|  |=====

  def error(error1: Throwable, errorN: Throwable*): IO[Nothing] =
    IO.wrapEffect { Dead(error1 :: errorN.toList) }

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

  object instances {

    given ioMonad: Monad[IO] with {

      extension [A](t: IO[A]) {

        def map[B](f: A => B): IO[B] = {
          import ErrorAccumulator.instances.given
          IO.wrapEffect { t.execute().map(f) }
        }

        def apply[B](f: IO[A => B]): IO[B] = {
          import ErrorAccumulator.instances.given
          IO.wrapEffect {
            val evaledT = t.execute()
            val evaledF = f.execute()

            evaledT.apply(evaledF)
          }
        }

      }

      def pure[I](i: => I): IO[I] =
        IO(i)

      // TODO (KR) : Can possibly be improved (?)
      extension [A](t: IO[IO[A]])
        def flatten: IO[A] = {
          import ErrorAccumulator.instances.given
          IO.wrapEffect { t.execute().flatMap(_.execute()) }
        }

    }

    given ioTraverseList: Traverse[List, IO] with {

      def traverse[A](t: List[IO[A]]): IO[List[A]] = {
        import ErrorAccumulator.instances.given
        IO.wrapEffect { t.map(_.runSync).traverse }
      }

    }

  }

}
