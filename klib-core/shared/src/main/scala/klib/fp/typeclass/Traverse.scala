package klib.fp.typeclass

trait Traverse[I[_], O[_]] {

  def traverse[A](t: I[O[A]]): O[I[A]]

}

object Traverse {

  object extensions {

    implicit class TraverseOps[I[_], O[_], T](t: I[O[T]])(implicit trav: Traverse[I, O]) {

      def traverse: O[I[T]] = trav.traverse(t)

    }

  }

}
