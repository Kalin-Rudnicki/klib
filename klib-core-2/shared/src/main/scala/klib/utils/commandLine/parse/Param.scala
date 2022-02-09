package klib.utils.commandLine.parse

sealed abstract class Param(val formattedName: String, _ps1: String, _allParamStrings: String*) {
  def allParamStrings: List[String] = _ps1 :: _allParamStrings.toList
}
object Param {
  final case class Short(name: Char) extends Param(s"-$name", s"-$name")
  final case class Long(name: String) extends Param(s"--$name", s"--$name")
  final case class ShortWithValue(name: Char) extends Param(s"-$name=VALUE", s"-$name")
  final case class LongWithValue(name: String) extends Param(s"--$name=VALUE", s"--$name")
  final case class ShortToggle(trueName: Char, falseName: Char) extends Param(s"-($trueName/$falseName)", s"-$trueName", s"-$falseName")
  final case class LongToggle(base: String, truePrefix: Option[String], falsePrefix: Option[String])
      extends Param(
        (truePrefix, falsePrefix) match {
          case (Some(truePrefix), Some(falsePrefix)) => s"--($truePrefix/$falsePrefix)-$base"
          case (Some(truePrefix), None)              => s"--[$truePrefix-]$base"
          case (None, Some(falsePrefix))             => s"--[$falsePrefix-]$base"
          case (None, None)                          => s"--$base"
        },
        truePrefix.fold(s"--$base")(p => s"--$p-$base"),
        falsePrefix.fold(s"--$base")(p => s"--$p-$base"),
      ) {
    def trueName: String = truePrefix.fold(base)(p => s"$p-$base")
    def falseName: String = falsePrefix.fold(base)(p => s"$p-$base")
  }
}
