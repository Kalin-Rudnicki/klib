package klib.utils.commandLine.parse

import cats.data.NonEmptyList
import cats.syntax.option.*

import klib.utils.*

final case class Error(
    element: Option[Element],
    reason: Error.Reason,
) {

  def toMessage: Message =
    reason match {
      case Error.Reason.MissingRequired(primaryParamName) =>
        Message.same(s"Missing required param: $primaryParamName")
      case Error.Reason.MalformattedValue(value, cause) =>
        Message.same(s"Malformatted value ${value.unesc}", cause.some)
      case Error.Reason.UnexpectedArg(arg) =>
        Message.same(s"Unexpected arg ${arg.value} at index ${arg.index}")
      case Error.Reason.ViolatedExclusiveOr(names) =>
        Message.same(s"You can only provide one of: ${names.mkString(", ")}")
    }

}
object Error {

  sealed trait Reason
  object Reason {
    final case class MissingRequired(primaryParamName: String) extends Reason
    final case class MalformattedValue(value: String, cause: Message) extends Reason
    final case class UnexpectedArg(arg: Indexed[Arg]) extends Reason
    final case class ViolatedExclusiveOr(names: Set[String]) extends Reason
  }

}
