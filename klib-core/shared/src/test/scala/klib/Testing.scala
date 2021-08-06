package klib

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

object Testing extends App {

  def doTraverse[F[_], T](
      w: List[F[T]],
  )(implicit
      fTraverseList: Traverse[List, F],
  ): F[List[T]] =
    w.traverse

}
