package klib.fp

package object typeclass {

  trait Implicits
      extends Functor.Implicits
      with Applicative.Implicits
      with Monad.Implicits
      with Foreach.Implicits
      with Traverse.Implicits
      with DecodeString.Implicits
      with EncodeToString.Implicits
  object Implicits extends Implicits

  trait Instances extends Functor.Instances with Applicative.Instances with Monad.Instances with Foreach.Instances
  object Instances extends Instances

}
