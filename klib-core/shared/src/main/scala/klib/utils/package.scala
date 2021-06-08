package klib

package object utils {

  trait Implicits extends ColorString.Implicits
  object Implicits extends Implicits

  val AnsiEscapeString: String = "\u001b["

}
