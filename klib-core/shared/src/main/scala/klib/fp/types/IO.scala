package klib.fp.types

import java.awt.image.BufferedImage
import java.io.{File, PrintWriter, RandomAccessFile}
import javax.imageio.ImageIO

import scala.io.Source
import scala.util.Try
import scala.sys.process._

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.utils.ado
import klib.utils._

final class IO[+T](val execute: () => ?[T]) {

  def runSync: ?[T] =
    execute()

  def toAsyncIO: AsyncIO[T] =
    AsyncIO.wrapIO(this)

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
          logger.log(L(runSyncErrors.map(L.log.throwable(_)))).runSync match {
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

  // =====|  |=====

  // rename? (do AsyncIO as well)
  def unLift: IO[?[T]] =
    IO { execute() }

  def bracket[T2](`try`: T => IO[T2])(`finally`: T => IO[Unit]): IO[T2] =
    this
      .flatMap { self =>
        ado[IO]
          .join(
            `try`(self),
            `finally`(self),
          )
          .map(_._1)
      }

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

  def errorMessage(error0: String, errorN: String*): IO[Nothing] =
    IO.wrapEffect { Dead((error0 :: errorN.toList).map(Message(_))) }

  def error(error0: Throwable, errorN: Throwable*): IO[Nothing] =
    IO.wrapEffect { Dead(error0 :: errorN.toList) }

  def errorIf(pred: Boolean)(error0: Throwable, errorN: Throwable*): IO[Unit] =
    if (pred) error(error0, errorN: _*)
    else ().pure[IO]

  def ??? : IO[Nothing] =
    ?.???.toIO

  def now: IO[Long] =
    System.currentTimeMillis.pure[IO]

  object syscall {

    def exitCode0(args: List[String]): IO[Unit] =
      for {
        res <- args.!.pure[IO]
        _ <- IO.errorIf(res != 0)(Message(s"syscall [${args.mkString(", ")}] returned non-0 exit-code"))
      } yield ()

    def exitCode0(arg: String): IO[Unit] =
      for {
        res <- arg.!.pure[IO]
        _ <- IO.errorIf(res != 0)(Message(s"syscall [$arg] returned non-0 exit-code"))
      } yield ()

    def exitCode0(arg0: String, arg1: String, argN: String*): IO[Unit] =
      exitCode0(arg0 :: arg1 :: argN.toList)

  }

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

  implicit val ioTraverseNonEmptyList: Traverse[NonEmptyList, IO] =
    new Traverse[NonEmptyList, IO] {

      override def traverse[T](t: NonEmptyList[IO[T]]): IO[NonEmptyList[T]] =
        IO.wrapEffect { t.map(_.runSync).traverse }

    }

}
