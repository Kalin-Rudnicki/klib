package klib.utils.commandLine.parse

sealed abstract class Param(val formattedName: String)
object Param {
  final case class Short(name: Char) extends Param(s"-$name")
  final case class Long(name: String) extends Param(s"--$name")
  final case class ShortWithValue(name: Char) extends Param(s"-$name=VALUE")
  final case class LongWithValue(name: String) extends Param(s"--$name=VALUE")
}
