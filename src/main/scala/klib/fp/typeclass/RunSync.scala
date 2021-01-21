package klib.fp.typeclass

import klib.fp.types._

trait RunSync[T[_], E] {

  def runSync[A](t: T[A]): E \/ A

}

object RunSync {

  trait Implicits {

    implicit class RunSyncOps[T[_], A, E](t: T[A])(implicit rs: RunSync[T, E]) {

      private val runSyncInstance: RunSync[T, E] = implicitly[RunSync[T, E]]

      def runSync: E \/ A =
        runSyncInstance.runSync(t)

    }

  }
  object Implicits extends Implicits

}
