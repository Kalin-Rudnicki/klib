package klib.fp.typeclass

trait Foreach[T[_]] {

  def foreach[A](t: T[A], f: A => Unit): Unit

}

object Foreach {

  trait Implicits {

    implicit class ForeachOps[T[_]: Foreach, A](t: T[A]) {

      private val foreachInstance = implicitly[Foreach[T]]

      def foreach(f: A => Unit): Unit =
        foreachInstance.foreach(t, f)

    }

  }
  object Implicits extends Implicits

  trait Instances {

    // TODO (KR) :

  }
  object Instances extends Instances

}
