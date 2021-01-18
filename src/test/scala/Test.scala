//

import klib.fp.types._
import klib.Implicits._
import klib.utils._

object Test extends App {

  final case class Node(
      id: Int,
      map: Map[Char, Lazy[Node]],
  )

  lazy val n0: Node =
    Node(
      0,
      Map(
        'A' -> Lazy(n0),
        'B' -> Lazy(n1),
      ),
    )

  lazy val n1: Node =
    Node(
      1,
      Map(
      ),
    )

}
