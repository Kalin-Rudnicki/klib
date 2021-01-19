package klib

import klib.utils.{Lazy, Logger}

object Testing extends App {

  final case class Box(
      id: Int,
      items: Map[Int, Lazy[Box]],
  )

  object Box {

    def apply(id: Int, items: (Int, Lazy[Box])*): Box =
      Box(id, items.toMap)

  }

  val boxes: List[Box] = {
    lazy val lazyMap: Map[Int, Box] =
      Map(
        0 ->
          Box(
            0,
            0 -> Lazy(lazyMap(0)),
            1 -> Lazy(lazyMap(1)),
          ),
        1 ->
          Box(
            1,
            1 -> Lazy(lazyMap(1)),
          ),
      )

    lazyMap.toList.map(_._2)
  }

  val logger = Logger(Logger.LogLevel.Debug)

  logger() { src =>
    boxes.foreach { box =>
      src.debug(box.id)
      src.indented() { src =>
        box.items.foreach {
          case (k, v) =>
            src.debug(s"$k => ${v.value.id}")
        }
      }
    }
  }

}
