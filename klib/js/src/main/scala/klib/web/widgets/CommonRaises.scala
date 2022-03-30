package klib.web.widgets

object CommonRaises {

  sealed trait SubmitOr[+O]
  object SubmitOr {
    case object Submit extends SubmitOr[Nothing]
    case class Or[O](value: O) extends SubmitOr[O]
  }

  type Submit = SubmitOr.Submit.type

}
