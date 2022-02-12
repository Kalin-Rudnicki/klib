package klib.utils.commandLine.parse

import cats.data.NonEmptyList

import klib.utils.*

final case class Error(
    element: Option[Element],
    reason: Error.Reason,
) {

  // TODO (KR) :
  def toMessage: Message =
    Message(toString)

}
object Error {

  sealed trait Reason
  object Reason {
    case object MissingRequired extends Reason
    final case class MalformattedValue(value: String) extends Reason
    final case class UnexpectedArg(arg: Indexed[Arg]) extends Reason
    final case class ViolatedExclusiveOr(names: Set[String]) extends Reason
  }

}
