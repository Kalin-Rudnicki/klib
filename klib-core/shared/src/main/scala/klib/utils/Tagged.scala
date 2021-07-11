package klib.utils

type Tagged[U] = { type Tag = U }
type @@[+T, U] = T with Tagged[U] { type Tagged <: T }

object Tagged {

  object extensions {

    // TODO (KR) : It would be nice to have [W <: T @@ _], but that doesnt seem to work anymore...
    extension [T](t: T) {
      inline def wrap[W]: W = t.asInstanceOf[W]
      inline def _wrap[W]: W = t.asInstanceOf[W]
    }

    extension [T, U](w: T @@ U) {

      inline def unwrap: T = w.asInstanceOf[T]

    }

  }

}
