package klib.utils.commandLine.parse

import klib.utils.{*, given}

final case class LeftLine(
    message: ColorString,
    indentCount: Int = 0,
) {
  def indentBy(by: Int): LeftLine = LeftLine(message, indentCount + by)
  def toColorString(leftPadding: Int): ColorString = color"${" " * (indentCount * leftPadding)}$message"
}
