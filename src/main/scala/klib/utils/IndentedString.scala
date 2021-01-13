package klib.utils

sealed trait IndentedString {

  // TODO (KR) : Possibly optimize this
  def toString(idtStr: String): String = {
    val stringBuilder = new StringBuilder
    var first = true

    def rec(
        indentedString: IndentedString,
        idt: Int,
    ): Unit = {
      def appendIndent: Unit = {
        if (first)
          first = false
        else
          stringBuilder.append('\n')
        0.until(idt).foreach(_ => stringBuilder.append(idtStr))
      }

      indentedString match {
        case IndentedString.Break =>
          appendIndent
        case IndentedString.Str(str) =>
          appendIndent
          stringBuilder.append(str)
        case IndentedString.Inline(children) =>
          children.foreach(rec(_, idt))
        case IndentedString.Indented(children) =>
          children.foreach(rec(_, idt + 1))
      }
    }

    rec(this, 0)
    stringBuilder.toString
  }

  override def toString: String = toString("    ")

}

object IndentedString {

  // =====| ADT |=====

  case object Break extends IndentedString

  final case class Str(str: String) extends IndentedString

  final case class Inline(children: List[IndentedString]) extends IndentedString
  object Inline {

    def apply(children: IndentedString*): Inline =
      Inline(children.toList)

  }

  final case class Indented(children: List[IndentedString]) extends IndentedString
  object Indented {

    def apply(children: IndentedString*): Indented =
      Indented(children.toList)

  }

}
