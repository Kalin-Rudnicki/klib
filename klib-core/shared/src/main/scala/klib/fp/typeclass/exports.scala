package klib.fp.typeclass

object extensions {
  export Applicative.extensions.{given, _}
  export DecodeString.extensions.{given, _}
  export Traverse.extensions.{given, _}
}

object instances {
  export DecodeString.instances.{given, _}
  export Monad.instances.{given, _}
}
