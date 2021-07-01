package klib.fp.types

// TODO (KR) : Add `cause`
final case class Message(message: String) extends Throwable(message) {

  override def toString: String =
    s"Message($message)"

}

final case class Compound(children: List[Throwable]) extends Throwable(children.map(_.getMessage).mkString("\n"))
