package klib.utils.commandLine.parse

import cats.syntax.option.*

final case class Indexed[+V](
    value: V,
    index: Int,
) {
  inline def map[V2](f: V => V2): Indexed[V2] = Indexed(f(value), index)
  inline def mapToList[V2](f: V => List[V2]): List[Indexed[V2]] =
    f(value).map(Indexed(_, index))
}
object Indexed {

  def list[V](list: List[V]): List[Indexed[V]] =
    list.zipWithIndex.map(Indexed.apply)

}

extension [V](v: V) {
  inline def atIndex(i: Int): Indexed[V] = Indexed(v, i)
}
