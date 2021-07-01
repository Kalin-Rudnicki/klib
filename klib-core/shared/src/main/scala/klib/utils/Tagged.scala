package klib.utils

type Tagged[U] = { type Tag = U }
type @@[+T, U] = T with Tagged[U] { type Tagged <: T }

object Tagged {

  object extensions {

    extension [T](t: T) {
      def wrap[W <: @@[T, _]]: W = t.asInstanceOf[W]
      def _wrap[W <: @@[T, _]]: W = t.asInstanceOf[W]
    }

    extension [T](w: T @@ _) {

      def unwrap: T = w

    }

  }

}
