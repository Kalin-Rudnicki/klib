package klib.utils

import cats.syntax.option.*

extension (str: String) {

  // =====| Misc |=====

  def toNES: Option[String] =
    Option.when(str.nonEmpty)(str)

  def pluralize(amount: Long, pluralSuffix: String = "s", singularSuffix: String = ""): String =
    s"$str${if (amount == 1) singularSuffix else pluralSuffix}"

  // =====| Alignment |=====

  private def alignFunction(length: Int, char: Char)(padF: Int => (Option[Int], Option[Int])): String = {
    val toAdd = length - str.length
    val charStr = char.toString
    if (toAdd > 0) {
      val (left, right) = padF(toAdd)
      List(left.map(charStr * _), str.some, right.map(charStr * _)).flatten.mkString
    } else str
  }

  def alignLeft(length: Int, char: Char = ' '): String =
    alignFunction(length, char)(toAdd => (None, toAdd.some))

  def alignRight(length: Int, char: Char = ' '): String =
    alignFunction(length, char)(toAdd => (toAdd.some, None))

  def alignCenter(length: Int, char: Char = ' '): String =
    alignFunction(length, char) { toAdd =>
      val left = toAdd / 2
      val right = toAdd - left
      (left.some, right.some)
    }

  // =====| Color |=====

  def stripColor: String =
    str.replaceAll("\u001b\\[\\d+(;\\d+)*m", "")

  def replaceColor(regex: String, color: Color): String =
    str.replaceAll(regex, s"\u001b[${color.fgMod}m$$0\u001b[0m")

}

implicit class UnescCharOps(char: Char) {

  def unesc: String =
    unesc("'")

  // TODO (KR) : Add more escaped characters here as they become known
  def unesc(_1: String, _2: Option[String] = None): String = {
    val left = _1
    val right = _2.getOrElse(_1)
    val charText = char match {
      case '\n' =>
        "\\n"
      case '\\' =>
        "\\\\"
      case '\t' =>
        "\\t"
      case '"' =>
        "\\\""
      case c =>
        c.toString
    }
    s"$left$charText$right"
  }

}

implicit class UnescStringOps(string: String) {

  def unesc: String =
    unesc("\"")

  def unesc(_1: String, _2: Option[String] = None, f: String => String = s => s): String = {
    // `f` serves the purpose of if you want to wrap in quotes, but also have other text inside the quotes
    val left = _1
    val right = _2.getOrElse(_1)
    val strText = f(string.map(_.unesc("")).mkString)
    s"$left$strText$right"
  }

}
