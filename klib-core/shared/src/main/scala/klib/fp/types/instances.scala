package klib.fp.types

object instances {
  export Either.instances.{given, _}
  export ErrorAccumulator.instances.{given, _}
  export IO.instances.{given, _}
  export Maybe.instances.{given, _}
  export NonEmptyList.instances.{given, _}
}
