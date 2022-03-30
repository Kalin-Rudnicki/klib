package klib.utils

implicit class OptionOps[O](self: Option[O]) {

  def cata[A](none: => A, some: O => A): A =
    self match {
      case None    => none
      case Some(o) => some(o)
    }

}
