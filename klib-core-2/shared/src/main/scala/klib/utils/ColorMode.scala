package klib.utils

sealed trait ColorMode {
  def selectColor(extended: Color, simple: Color): Color
}
object ColorMode {
  case object Extended extends ColorMode {
    override def selectColor(extended: Color, simple: Color): Color = extended
  }
  case object Simple extends ColorMode {
    override def selectColor(extended: Color, simple: Color): Color = simple
  }
  case object Colorless extends ColorMode {
    override def selectColor(extended: Color, simple: Color): Color = Color.Default
  }
}
