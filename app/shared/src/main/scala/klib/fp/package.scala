package klib

package object fp {

  trait Implicits extends typeclass.Implicits with types.Implicits
  object Implicits extends Implicits

}
