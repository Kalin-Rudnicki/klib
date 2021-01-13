package klib.fp

package object typeclass {

  trait Implicits extends Functor.Implicits with Applicative.Implicits with Monad.Implicits with ForEach.Implicits
  object Implicits extends Implicits

  trait Instances extends Functor.Instances with Applicative.Instances with Monad.Instances with ForEach.Instances
  object Instances extends Instances

}
