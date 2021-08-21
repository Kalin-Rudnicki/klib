package klib

import klib.fp.Implicits._
import klib.fp.types._

package object utils {

  trait Implicits extends ColorString.Implicits with Logger.Event.Implicits {

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

    // ---  ---

    implicit class AlignStringOps(str: String) {

      def leftAlign(width: Int, padChar: Char = ' '): String =
        s"$str${padChar.toString * (width - str.length).max(0)}"

      def rightAlign(width: Int, padChar: Char = ' '): String =
        s"${padChar.toString * (width - str.length).max(0)}$str"

      def centerAlign(width: Int, padChar: Char = ' '): String = {
        val extra = (width - str.length).max(0)
        val left = extra / 2
        val right = extra - left
        s"${padChar.toString * left}$str${padChar.toString * right}"
      }

    }

  }
  object Implicits extends Implicits

  type Tagged[U] = { type Tag = U }
  type @@[+T, U] = T with Tagged[U] { type Tagged <: T }

  val AnsiEscapeString: String = "\u001b["

}
