package klib.fp.typeclass

trait Monad[T[_]] extends Applicative[T] {

  def flatMap[A, B](t: T[A], f: A => T[B]): T[B]

  final def flatten[A](t: T[T[A]]): T[A] = flatMap[T[A], A](t, identity)

  final def flatApply[A, B](t: T[A], f: T[A => T[B]]): T[B] =
    flatten(apply(t, f))

}

object Monad {

  trait Implicits {

    implicit class MonadOps[T[_]: Monad, A](t: T[A]) { // extends Applicative.Implicits.ApplicativeOps(t) {
      import Functor.Implicits._

      private val monad: Monad[T] = implicitly[Monad[T]]

      def flatMap[B](f: A => T[B]): T[B] =
        monad.flatMap(t, f)

      def <*[B](t2: T[B]): T[A] =
        flatMap { a => t2.map { _ => a } }

      def *>[B](t2: T[B]): T[B] =
        flatMap { _ => t2 }

      def <*>[B](t2: T[B]): T[(A, B)] =
        flatMap { a => t2.map((a, _)) }

      def <**[B](f: A => T[B]): T[A] =
        flatMap { a => f(a).map { _ => a } }

      def **>[B](f: A => T[B]): T[B] =
        flatMap(f)

      def <**>[B](f: A => T[B]): T[(A, B)] =
        flatMap { a => f(a).map((a, _)) }

    }

    implicit class NestedMonadOps[T[_]: Monad, A](t: T[T[A]]) {

      private val monad: Monad[T] = implicitly[Monad[T]]

      def flatten: T[A] = monad.flatten(t)

    }

  }
  object Implicits extends Implicits

  trait Instances {
    import scala.concurrent.{ExecutionContext, Future, Promise}
    import scala.util.{Failure, Success}

    implicit def futureMonad(implicit ec: ExecutionContext): Monad[Future] =
      new Monad[Future] {
        override def map[A, B](t: Future[A], f: A => B): Future[B] = {
          val p: Promise[B] = Promise()

          t.onComplete {
            case Failure(exception) =>
              p.failure(exception)
            case Success(value) =>
              p.success(f(value))
          }

          p.future
        }

        override def apply[A, B](t: Future[A], f: Future[A => B]): Future[B] = {
          val p: Promise[B] = Promise()

          f.onComplete {
            case Failure(fException) =>
              p.failure(fException)
            case Success(fValue) =>
              t.onComplete {
                case Failure(tException) =>
                  p.failure(tException)
                case Success(tValue) =>
                  p.success(fValue(tValue))
              }
          }

          p.future
        }

        override def pure[A](a: => A): Future[A] =
          Future(a)

        override def flatMap[A, B](t: Future[A], f: A => Future[B]): Future[B] = {
          val p: Promise[B] = Promise()

          t.onComplete {
            case Failure(exception) =>
              p.failure(exception)
            case Success(t2) =>
              f(t2).onComplete {
                case Failure(exception) =>
                  p.failure(exception)
                case Success(value) =>
                  p.success(value)
              }
          }

          p.future
        }

      }

  }
  object Instances extends Instances

}
