package klib.utils.commandLine.parse

import cats.data.EitherNel
import cats.data.NonEmptyList

final case class Result[+T](
    res: EitherNel[Error, T],
    remainingArgs: List[Arg],
) {

  def mapResult[T2](f: T => T2): Result[T2] =
    Result(res.map(f), remainingArgs)

  def flatMapResult[T2](f: T => EitherNel[Error, T2]): Result[T2] =
    Result(res.flatMap(f), remainingArgs)

}
