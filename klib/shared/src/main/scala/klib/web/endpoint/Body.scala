package klib.web.endpoint

/**
  * This is only used to specify the endpoint type,
  * and is never meant to actually be instantiated.
  */
sealed trait Body
object Body {
  final class None private extends Body
  final class Raw private extends Body
  final class Encoded[O] private extends Body
  final class File private extends Body
}
