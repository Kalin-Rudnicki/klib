package klib.utils.commandLine.parse

import cats.data.NonEmptyList
import cats.syntax.option.*

import klib.utils.*

final case class Error(
    element: Option[Element],
    reason: Error.Reason,
) {

  def toKError: KError =
    reason match {
      case Error.Reason.MissingRequired(primaryParamName) =>
        KError.UserError(s"Missing required param: $primaryParamName")
      case Error.Reason.MalformattedValue(value, cause) =>
        KError.UserError(s"Malformatted value ${value.unesc}", KError.WrappedKErrors(cause))
      case Error.Reason.UnexpectedArg(arg) =>
        KError.UserError(s"Unexpected arg ${arg.value} at index ${arg.index}")
      case Error.Reason.ViolatedExclusiveOr(names) =>
        KError.UserError(s"You can only provide one of: ${names.mkString(", ")}")
    }

}
object Error {

  sealed trait Reason
  object Reason {
    final case class MissingRequired(primaryParamName: String) extends Reason
    final case class MalformattedValue(value: String, cause: NonEmptyList[KError]) extends Reason
    final case class UnexpectedArg(arg: Indexed[Arg]) extends Reason
    final case class ViolatedExclusiveOr(names: Set[String]) extends Reason
  }

}
