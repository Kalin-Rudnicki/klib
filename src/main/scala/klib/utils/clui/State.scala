package klib.utils.clui

object State {
  sealed trait Output[+T]
  sealed trait Input[+T] extends Output[T]

  case object Exit extends Output[Nothing]
  case object Unknown extends Output[Nothing]
  case object Empty extends Input[Nothing]
  final case class Custom[+V](value: V) extends Input[V]
}
