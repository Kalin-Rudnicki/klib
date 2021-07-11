package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  final case class PreState(map: Map[Char, Int], isValidFinish: Boolean)
  final case class State(id: Int, map: Map[Char, Lazy[State]], isValidFinish: Boolean) {
    override def toString: String =
      s"State($id, ${map.map { case (k, v) => (k, s"State[${v.value.id}, ${v.value.isValidFinish}]") }}, $isValidFinish)"
  }

  val preStates: List[PreState] =
    List(
      PreState(
        Map(
          'A' -> 0,
          'B' -> 1,
        ),
        false,
      ),
      PreState(
        Map(
          'A' -> 0,
          'C' -> 2,
        ),
        false,
      ),
      PreState(
        Map(
        ),
        true,
      ),
    )

  val states: List[State] =
    Lazy
      .selfMap[(PreState, Int), Int, State](preStates.zipWithIndex) {
        case ((s, i), ef) =>
          (
            i,
            State(
              id = i,
              map = s.map.map {
                case (k, v) =>
                  (
                    k,
                    ef(v),
                  )
              },
              isValidFinish = s.isValidFinish,
            ),
          )
      }
      .values
      .toList

  println
  println("=====| PreState |=====")
  preStates.foreach(println)

  println
  println("=====| State |=====")
  states.foreach(println)

}
