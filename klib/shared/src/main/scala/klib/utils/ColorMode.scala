package klib.utils

import cats.data.NonEmptyList

opaque type AnsiMod = String
object AnsiMod {
  def apply(str: String): AnsiMod = str
}

sealed trait ColorMode {
  def selectColor(extended: Color, simple: Color): Color
  protected def ansiPrefix: String
  protected def ansiSuffix: String
  def fgMod(color: Color): AnsiMod
  def bgMod(color: Color): AnsiMod
  final def ansiEscape(modifiers: NonEmptyList[AnsiMod]): String = modifiers.toList.mkString(ansiPrefix, ";", ansiSuffix)
}
object ColorMode {
  sealed trait Standard extends ColorMode {
    override protected final def ansiPrefix: String = AnsiEscapeString
    override protected final def ansiSuffix: String = "m"
    override final def fgMod(color: Color): AnsiMod = AnsiMod(color.fgMod)
    override final def bgMod(color: Color): AnsiMod = AnsiMod(color.bgMod)
  }

  case object Extended extends Standard {
    override def selectColor(extended: Color, simple: Color): Color = extended
  }
  case object Simple extends Standard {
    override def selectColor(extended: Color, simple: Color): Color = simple
  }
  case object Colorless extends Standard {
    override def selectColor(extended: Color, simple: Color): Color = Color.Default
  }
  case object Show extends ColorMode {
    override def selectColor(extended: Color, simple: Color): Color = extended
    override protected def ansiPrefix: String = "[["
    override protected def ansiSuffix: String = "]]"
    override def fgMod(color: Color): AnsiMod =
      color match {
        case Color.RGB(r, g, b) => AnsiMod(s"rgb($r, $g, $b)")
        case _                  => AnsiMod(color.toString)
      }
    override def bgMod(color: Color): AnsiMod =
      color match {
        case Color.RGB(r, g, b) => AnsiMod(s"rgbGB($r, $g, $b)")
        case _                  => AnsiMod(s"${color}BG")
      }
  }
}
