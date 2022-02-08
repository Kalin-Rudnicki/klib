package klib.utils.commandLine.parse

sealed abstract class Param(val formattedName: String)
object Param {
  final case class SimpleShort(name: Char) extends Param(s"-$name")
  final case class SimpleLong(name: String) extends Param(s"--$name")
}
