package klib.fp.typeclass

trait Monad[T[_]] extends Applicative[T] {

  extension[A](t: T[T[A]]) def flatten: T[A]
  
  extension[A](t: T[A]) {
    
    def flatMap[B](f: A => T[B]): T[B] =
      t.map(f).flatten
      
    def flatApply[B](f: T[A => T[B]]): T[B] =
      t.apply(f).flatten
      
  }

}

object Monad {

  object instances {
    import scala.concurrent._
    import scala.util._

    given futureMonad(using ec: ExecutionContext): Monad[Future] with {

      extension[A](t: Future[A]) {

        def map[B](f: A => B): Future[B] = {
          val p: Promise[B] = Promise()

          t.onComplete {
            case Failure(exception) =>
              p.failure(exception)
            case Success(value) =>
              p.success(f(value))
          }

          p.future
        }

        def apply[B](f: Future[A => B]): Future[B] = {
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

      }

      extension[I](i: => I) def pure: Future[I] = Future(i)

      extension[A](t: Future[Future[A]]) def flatten: Future[A] = {
        val p: Promise[A] = Promise()

        t.onComplete {
          case Failure(exception) =>
            p.failure(exception)
          case Success(t2) =>
            t2.onComplete {
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

}
