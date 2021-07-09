package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  val color = ColorString.Color(None, Color.Named.Black.some)

  print("123")
  color.toColorState.colorizeAndDeColorize(ColorString.ColorState.Default) match {
    case Some((colorize, deColorize)) =>
      print(colorize)
      print("456")
      print(deColorize)
    case None =>
      print("456")
  }
  print("789")
  println

}
