package klib.fp

package object types {

  type \/[+A, +B] = Either[A, B]

  type ?[+R] = ErrorAccumulator[Throwable, Throwable, R]

  final case class Message(message: String) extends Throwable(message) {

    override def toString: String =
      s"Message($message)"

  }

  trait Implicits extends Maybe.Implicits with Either.Implicits with ErrorAccumulator.Implicits
  object Implicits extends Implicits

}
