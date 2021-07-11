package klib.utils

type Tagged[U] = { type Tag = U }
type @@[+T, U] = T with Tagged[U] { type Tagged <: T }

object Tagged {

  object extensions {

    extension [T](t: T) {
      inline def wrap[W]: W = t.asInstanceOf[W]
      inline def _wrap[W]: W = t.asInstanceOf[W]
    }

    extension [T, U](w: T @@ U) {

      inline def unwrap: T = w.asInstanceOf[T]

    }

  }

}
