package klib.fp.types

import scala.concurrent._
import scala.concurrent.duration.Duration

import klib.Implicits._
import klib.fp.typeclass._

final class AsyncIO[+T](val execute: () => Future[?[T]]) {

  def runASync(onComplete: ?[T] => Unit)(implicit ec: ExecutionContext): Unit =
    execute().onComplete { res =>
      onComplete(res.to_?.flatten)
    }

  def toIO(duration: Maybe[Duration])(implicit ec: ExecutionContext): IO[T] =
    IO.wrapEffect {
      Await.result(execute(), duration.getOrElse(Duration.Inf))
    }

}

object AsyncIO {

  def apply[T](t: => T)(implicit ec: ExecutionContext): AsyncIO[T] =
    AsyncIO.wrapIO(IO(t))

  def wrapIO[T](t: IO[T])(implicit ec: ExecutionContext): AsyncIO[T] =
    AsyncIO.wrapEffect(t.execute())

  def wrapEffect[T](t: => ?[T])(implicit ec: ExecutionContext): AsyncIO[T] =
    AsyncIO.wrapWrappedEffect(Future(t))

  def wrapFuture[T](t: => Future[T])(implicit ec: ExecutionContext): AsyncIO[T] =
    wrapWrappedEffect(t.map(_.pure[?]))

  def wrapWrappedEffect[T](t: => Future[?[T]]): AsyncIO[T] =
    new AsyncIO[T](() => t)

  // =====|  |=====

  def runSequentially[T](ts: List[AsyncIO[T]])(implicit ec: ExecutionContext): AsyncIO[List[T]] =
    ts match {
      case head :: tail =>
        for {
          h <- head
          t <- runSequentially(tail)
        } yield h :: t
      case Nil =>
        Nil.pure[AsyncIO]
    }

  def runParallel[T](ts: List[AsyncIO[T]])(implicit ec: ExecutionContext): AsyncIO[List[T]] =
    AsyncIO.wrapWrappedEffect {
      Future
        .traverse(ts)(_.execute())
        .map(_.traverse)
    }

  // =====|  |=====

  implicit def asyncIOMonad(implicit ec: ExecutionContext): Monad[AsyncIO] =
    new Monad[AsyncIO] {

      override def map[A, B](t: AsyncIO[A], f: A => B): AsyncIO[B] =
        AsyncIO.wrapWrappedEffect {
          val p: Promise[?[B]] = Promise()

          t.execute().onComplete { t =>
            p.success(t.to_?.flatten.map(f))
          }

          p.future
        }

      override def apply[A, B](t: AsyncIO[A], f: AsyncIO[A => B]): AsyncIO[B] =
        AsyncIO.wrapWrappedEffect {
          val p: Promise[?[B]] = Promise()

          val tE = t.execute()
          val fE = f.execute()

          tE.onComplete { t =>
            fE.onComplete { f =>
              p.success(t.to_?.flatten.apply(f.to_?.flatten))
            }
          }

          p.future
        }

      override def pure[A](a: => A): AsyncIO[A] =
        AsyncIO(a)

      override def flatMap[A, B](t: AsyncIO[A], f: A => AsyncIO[B]): AsyncIO[B] =
        AsyncIO.wrapWrappedEffect {
          val p: Promise[?[B]] = Promise()

          t.execute().onComplete { t =>
            t.to_?.flatten match {
              case Alive(r) =>
                f(r).execute().onComplete { f =>
                  p.success(f.to_?.flatten)
                }
              case dead @ Dead(_) =>
                p.success(dead)
            }
          }

          p.future
        }

    }

  implicit def asyncIOTraverseList(implicit ec: ExecutionContext): Traverse[List, AsyncIO] =
    new Traverse[List, AsyncIO] {

      override def traverse[T](t: List[AsyncIO[T]]): AsyncIO[List[T]] =
        AsyncIO.runParallel(t)

    }

}
