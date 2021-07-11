package klib.fp.types

import scala.util.Try

object extensions {
  export Maybe.extensions.{given, _}
  export Either.extensions.{given, _}
  export ErrorAccumulator.extensions.{given, _}
  export NonEmptyList.extensions.{given, _}

  extension [I](i: I) {

    def someOpt: Option[I] =
      scala.Some(i)

  }

  extension [A](t: Option[A]) {

    def toMaybe: Maybe[A] =
      t match {
        case scala.Some(value) => Some(value)
        case scala.None        => None
      }

  }

  extension [L, R](t: scala.Either[L, R]) {

    def toEA: ErrorAccumulator[L, R] =
      t match {
        case scala.Right(value) => Alive(value)
        case scala.Left(value)  => Dead(value :: Nil)
      }

    def to_\/ : Either[L, R] =
      t match {
        case scala.Right(value) => Right(value)
        case scala.Left(value)  => Left(value)
      }

  }

  extension [A](t: Try[A]) {

    def to_?? : ??[A] =
      t.toEither.toEA

    def to_\/ : Either[Throwable, A] =
      t.toEither.to_\/

  }

  final class AwaitingIfFalse[+A](b: Boolean, ifTrue: => A) {

    def |[A2 >: A](ifFalse: => A2): A2 =
      if (b) ifTrue
      else ifFalse

  }

  extension (b: Boolean) {

    def maybe[A](a: => A): Maybe[A] =
      if (b) Some(a)
      else None

    def ?[A](ifTrue: => A): AwaitingIfFalse[A] =
      new AwaitingIfFalse(b, ifTrue)

  }

}

object instances {
  export Either.instances.{given, _}
  export ErrorAccumulator.instances.{given, _}
  export IO.instances.{given, _}
  export Maybe.instances.{given, _}
  export NonEmptyList.instances.{given, _}
}
