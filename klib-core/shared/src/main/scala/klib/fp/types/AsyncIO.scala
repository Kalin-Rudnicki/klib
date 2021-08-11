package klib.fp.types

import scala.concurrent._
import scala.concurrent.duration.Duration

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.utils.ado

final class AsyncIO[+T](val execute: ExecutionContext => Future[?[T]]) {

  def runASync(onComplete: ?[T] => Unit)(implicit ec: ExecutionContext): Unit =
    execute(ec).onComplete { res =>
      onComplete(res.to_?.flatten)
    }(ec)
  def runASyncGlobal(onComplete: ?[T] => Unit): Unit =
    runASync(onComplete)(ExecutionContext.global)

  // TODO (KR) : Make sure this catches any sort of exception thrown by Await
  def toIO(duration: Maybe[Duration])(implicit ec: ExecutionContext): IO[T] =
    IO.wrapEffect {
      Await.result(execute(ec), duration.getOrElse(Duration.Inf))
    }
  def toIOGlobal(duration: Maybe[Duration]): IO[T] =
    toIO(duration)(ExecutionContext.global)

  // =====|  |=====

  def unLift: AsyncIO[?[T]] =
    AsyncIO.wrapFuture(execute)

  def bracket[T2](`try`: T => AsyncIO[T2])(`finally`: T => AsyncIO[Unit]): AsyncIO[T2] =
    this
      .flatMap { self =>
        ado[AsyncIO](AsyncIO.sequentialAsyncIOMonad)
          .join(
            `try`(self),
            `finally`(self),
          )
          .map(_._1)
      }

}

object AsyncIO {

  def apply[T](t: => T): AsyncIO[T] =
    AsyncIO.wrapIO(IO(t))

  def wrapIO[T](t: IO[T]): AsyncIO[T] =
    AsyncIO.wrapEffect(t.execute())

  def wrapEffect[T](t: => ?[T]): AsyncIO[T] =
    AsyncIO.wrapWrappedEffect(Future(t)(_))

  // NOTE : Wrapping an already created future has unknown behavior
  //      : bad :
  //      : val fT: Future[Int] =  Future(5)
  //      : val aT: AsyncIO[Int] = AsyncIO.wrapFuture(_ => fT)
  //      : good:
  //      : val aT: AsyncIO[Int] = AsyncIO.wrapFuture(Future(5)(_))
  def wrapFuture[T](t: ExecutionContext => Future[T]): AsyncIO[T] =
    wrapWrappedEffect { ec =>
      t(ec).map(_.pure[?])(ec)
    }

  def wrapWrappedEffect[T](t: ExecutionContext => Future[?[T]]): AsyncIO[T] =
    new AsyncIO[T](t)

  // =====|  |=====

  def runSequentially[T](ts: List[AsyncIO[T]]): AsyncIO[List[T]] =
    ts match {
      case head :: tail =>
        for {
          h <- head
          t <- runSequentially(tail)
        } yield h :: t
      case Nil =>
        Nil.pure[AsyncIO]
    }

  def runParallel[T](ts: List[AsyncIO[T]]): AsyncIO[List[T]] =
    AsyncIO.wrapWrappedEffect { implicit ec =>
      Future
        .traverse(ts)(_.execute(ec))
        .map(_.traverse)
    }

  // =====|  |=====

  sealed trait AsyncIOMonad extends Monad[AsyncIO] {

    override final def map[A, B](t: AsyncIO[A], f: A => B): AsyncIO[B] =
      AsyncIO.wrapWrappedEffect { ec =>
        val p: Promise[?[B]] = Promise()

        t.execute(ec)
          .onComplete { t =>
            p.success(t.to_?.flatten.map(f))
          }(ec)

        p.future
      }

    override final def pure[A](a: => A): AsyncIO[A] =
      AsyncIO(a)

    override final def flatMap[A, B](t: AsyncIO[A], f: A => AsyncIO[B]): AsyncIO[B] =
      AsyncIO.wrapWrappedEffect { ec =>
        val p: Promise[?[B]] = Promise()

        t.execute(ec)
          .onComplete { t =>
            t.to_?.flatten match {
              case Alive(r) =>
                f(r)
                  .execute(ec)
                  .onComplete { f =>
                    p.success(f.to_?.flatten)
                  }(ec)
              case dead @ Dead(_) =>
                p.success(dead)
            }
          }(ec)

        p.future
      }

  }

  val parallelAsyncIOMonad: Monad[AsyncIO] =
    new AsyncIOMonad {

      override def apply[A, B](t: AsyncIO[A], f: AsyncIO[A => B]): AsyncIO[B] =
        AsyncIO.wrapWrappedEffect { ec =>
          val p: Promise[?[B]] = Promise()

          val tE = t.execute(ec)
          val fE = f.execute(ec)

          tE.onComplete { t =>
            fE.onComplete { f =>
              p.success(t.to_?.flatten.apply(f.to_?.flatten))
            }(ec)
          }(ec)

          p.future
        }

    }

  val sequentialAsyncIOMonad: Monad[AsyncIO] =
    new AsyncIOMonad {

      override def apply[A, B](t: AsyncIO[A], f: AsyncIO[A => B]): AsyncIO[B] =
        AsyncIO.wrapWrappedEffect { ec =>
          val p: Promise[?[B]] = Promise()

          t.execute(ec)
            .onComplete { t =>
              f.execute(ec)
                .onComplete { f =>
                  p.success(t.to_?.flatten.apply(f.to_?.flatten))
                }(ec)
            }(ec)

          p.future
        }

    }

  implicit val defaultAsyncIOMonad: Monad[AsyncIO] = parallelAsyncIOMonad

  implicit val asyncIOTraverseList: Traverse[List, AsyncIO] =
    new Traverse[List, AsyncIO] {

      override def traverse[T](t: List[AsyncIO[T]]): AsyncIO[List[T]] =
        AsyncIO.runParallel(t)

    }

}
