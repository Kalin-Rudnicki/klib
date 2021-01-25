package klib.fp.typeclass

trait Traverse[I[_], O[_]] {

  def traverse[T](t: I[O[T]]): O[I[T]]

}

object Traverse {

  trait Implicits {

    implicit class TraverseOps[I[_], O[_], T](t: I[O[T]])(implicit traverseInstance: Traverse[I, O]) {

      def traverse: O[I[T]] =
        traverseInstance.traverse(t)

    }

  }
  object Implicits extends Implicits

}
