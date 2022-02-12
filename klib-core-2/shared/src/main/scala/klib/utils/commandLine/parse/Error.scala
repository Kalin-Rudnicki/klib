package klib.utils.commandLine.parse

import cats.data.NonEmptyList

final case class Error(
    element: Option[Element],
    reason: Error.Reason,
)
object Error {

  sealed trait Reason
  object Reason {
    case object MissingRequired extends Reason
    final case class MalformattedValue(value: String) extends Reason
    final case class UnexpectedArg(arg: Indexed[Arg]) extends Reason
  }

}
