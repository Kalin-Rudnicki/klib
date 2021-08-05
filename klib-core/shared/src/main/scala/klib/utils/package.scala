package klib

import klib.fp.Implicits._
import klib.fp.types._

package object utils {

  trait Implicits extends ColorString.Implicits with Logger.helpers.Implicits {

    implicit class TaggedIdOps[T](t: T) {
      def wrap[W <: @@[T, _]]: W = t.asInstanceOf[W]
      def _wrap[W <: @@[T, _]]: W = t.asInstanceOf[W]
    }

    implicit class TaggedOps[T](w: T @@ _) {
      def unwrap: T = w
    }

    implicit class ThrowableOps(throwable: Throwable) {

      def withMessage(msg: String): Throwable = {
        val thr = Message(msg)
        thr.setStackTrace(throwable.getStackTrace)
        thr
      }

      def mappedMessage(msgF: String => String): Throwable =
        withMessage(msgF(throwable.getMessage))

    }

  }
  object Implicits extends Implicits

  type Tagged[U] = { type Tag = U }
  type @@[+T, U] = T with Tagged[U] { type Tagged <: T }

  val AnsiEscapeString: String = "\u001b["

}
