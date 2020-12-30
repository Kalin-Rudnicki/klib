package klib.fp

package object typeclass {

  trait Implicits extends Functor.Implicits with Applicative.Implicits with Monad.Implicits
  object Implicits extends Implicits

  trait Instances extends Functor.Instances with Applicative.Instances with Monad.Instances
  object Instances extends Instances

}
