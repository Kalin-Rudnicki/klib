package klib.fp.typeclass

trait ForEach[T[_]] {

  def forEach[A](t: T[A], f: A => Unit): Unit

}

object ForEach {

  trait Implicits {

    implicit class ForEachOps[T[_]: ForEach, A](t: T[A]) {

      private val forEachInstance = implicitly[ForEach[T]]

      def forEach(f: A => Unit): Unit =
        forEachInstance.forEach(t, f)

    }

  }
  object Implicits extends Implicits

  trait Instances {

    // TODO (KR) :

  }
  object Instances extends Instances

}
