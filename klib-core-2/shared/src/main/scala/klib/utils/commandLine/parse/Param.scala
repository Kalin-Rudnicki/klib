package klib.utils.commandLine.parse

import klib.utils.{*, given}

sealed trait Param {
  def formattedName: ColorString
  def allParamStrings: List[String]
}
object Param {
  private def short(name: Char): String = s"-$name"
  private def long(name: String): String = s"--$name"

  private def formattedShort(name: String, value: Boolean): ColorString =
    if (value) color"${s"-$name".cyan}=${"VALUE".magenta}".dflt
    else color"${s"-$name".cyan}".dflt
  private def formattedLong(name: String, value: Boolean): ColorString =
    if (value) color"${s"--$name".cyan}=${"VALUE".magenta}".dflt
    else color"${s"--$name".cyan}".dflt

  final case class Short(name: Char) extends Param {
    override def formattedName: ColorString = formattedShort(name.toString, false)
    override def allParamStrings: List[String] = short(name) :: Nil
  }
  final case class Long(name: String) extends Param {
    override def formattedName: ColorString = formattedLong(name, false)
    override def allParamStrings: List[String] = long(name) :: Nil
  }
  final case class ShortWithValue(name: Char) extends Param {
    override def formattedName: ColorString = formattedShort(name.toString, true)
    override def allParamStrings: List[String] = short(name) :: Nil
  }
  final case class LongWithValue(name: String) extends Param {
    override def formattedName: ColorString = formattedLong(name, true)
    override def allParamStrings: List[String] = long(name) :: Nil
  }
  final case class ShortToggle(trueName: Char, falseName: Char) extends Param {
    override def formattedName: ColorString = formattedShort(s"($trueName/$falseName)", false)
    override def allParamStrings: List[String] = short(trueName) :: short(falseName) :: Nil
  }
  final case class LongToggle(base: String, truePrefix: Option[String], falsePrefix: Option[String]) extends Param {
    def trueName: String = truePrefix.fold(base)(p => s"$p-$base")
    def falseName: String = falsePrefix.fold(base)(p => s"$p-$base")
    override def formattedName: ColorString =
      formattedLong(
        (truePrefix, falsePrefix) match {
          case (Some(truePrefix), Some(falsePrefix)) => s"($truePrefix/$falsePrefix)-$base"
          case (Some(truePrefix), None)              => s"[$truePrefix-]$base"
          case (None, Some(falsePrefix))             => s"[$falsePrefix-]$base"
          case (None, None)                          => base
        },
        false,
      )
    override def allParamStrings: List[String] = long(trueName) :: long(falseName) :: Nil
  }
}
