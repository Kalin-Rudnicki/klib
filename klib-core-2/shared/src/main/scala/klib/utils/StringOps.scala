package klib.utils

import cats.syntax.option.*

extension (str: String) {

  // =====| Misc |=====

  def toNES: Option[String] =
    Option.when(str.nonEmpty)(str)

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
