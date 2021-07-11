package klib.predefs

object common {
  export klib.extensions.{given, _}
  export klib.instances.{given, _}
  export klib.fp.utils.ado
  export klib.utils.Logger.{helpers => L}

  // TODO (KR) : Hopefully do this in the future, but for now it is not allowed...
  // export klib.fp.types.{extensions => _, instances => _, _}
  // export klib.utils.{extensions => _, instances => _, _}

  // TODO (KR) : Make sure to add exports here if any new files are added

  // --- klib.fp.types._ ---
  // Either.scala
  export klib.fp.types.Either
  export klib.fp.types.Left
  export klib.fp.types.Right
  // ErrorAccumulator.scala
  export klib.fp.types.ErrorAccumulator
  export klib.fp.types.Alive
  export klib.fp.types.Dead
  export klib.fp.types.??
  // Errors.scala
  export klib.fp.types.Message
  export klib.fp.types.Compound
  // IO.scala
  export klib.fp.types.IO
  // Maybe.scala
  export klib.fp.types.Maybe
  export klib.fp.types.Some
  export klib.fp.types.None
  // NonEmptyList.scala
  export klib.fp.types.NonEmptyList
  // Trampoline.scala
  export klib.fp.types.Trampoline

  // --- klib.utils._ ---
  // Color.scala
  export klib.utils.Color
  // ColorString.scala
  export klib.utils.ColorString
  // IndentedString.scala
  export klib.utils.IndentedString
  // InfiniteSet.scala
  export klib.utils.InfiniteSet
  // Lazy.scala
  export klib.utils.Lazy
  // Logger.scala
  export klib.utils.Logger
  // package.scala
  export klib.utils.AnsiEscapeString
  // Pointer.scala
  export klib.utils.Pointer
  // Tagged.scala
  export klib.utils.Tagged
  // Timer.scala
  export klib.utils.Timer
  // Var.scala
  export klib.utils.Var
}
