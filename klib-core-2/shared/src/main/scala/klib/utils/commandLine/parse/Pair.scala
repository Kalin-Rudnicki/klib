package klib.utils.commandLine.parse

import scala.annotation.tailrec

opaque type Pair[+L, +R] = (L, R)
extension [L, R](pair: Pair[L, R]) {
  def left: L = pair._1
  def right: R = pair._2
}
object Pair {

  def apply[L, R](left: L, right: R): Pair[L, R] = (left, right)

  opaque type Same[+T] = Pair[T, T]
  object Same {

    def apply[T](left: T, right: T): Pair.Same[T] =
      Pair(left, right)

  }

  def zipPair(helpConfig: HelpConfig, pair: Pair.Same[List[String]]): List[Pair.Same[String]] = {
    @tailrec
    def loop(
        leftLines: List[String],
        rightLines: List[String],
        stack: List[(String, String)],
    ): List[Pair.Same[String]] =
      leftLines match {
        case leftHead :: leftTail =>
          if (leftHead.length > helpConfig.maxParamsWidth)
            loop(
              leftTail,
              rightLines,
              (leftHead, "") :: stack,
            )
          else
            rightLines match {
              case rightHead :: rightTail =>
                loop(
                  leftTail,
                  rightTail,
                  (leftHead, rightHead) :: stack,
                )
              case Nil =>
                loop(
                  leftTail,
                  rightLines,
                  (leftHead, "") :: stack,
                )
            }
        case Nil =>
          rightLines match {
            case rightHead :: rightTail =>
              loop(
                Nil,
                rightTail,
                ("", rightHead) :: stack,
              )
            case Nil =>
              stack.reverse
          }
      }

    loop(
      pair.left,
      pair.right,
      Nil,
    )
  }

  def zipPairs(helpConfig: HelpConfig)(pairs: List[Pair.Same[List[String]]]*): List[Pair.Same[String]] =
    pairs.toList.flatMap(_.flatMap(zipPair(helpConfig, _)))

  def makeLines(helpConfig: HelpConfig, pairs: List[Pair.Same[String]]): List[String] = {
    val maxParamsUsed: Int = pairs.map(_.left.length).maxOption.getOrElse(0).min(helpConfig.maxParamsWidth)

    pairs.map { pair =>
      s"${" " * helpConfig.leftPadding}${pair.left}${" " * (maxParamsUsed - pair.left.length + helpConfig.centerPadding)}${pair.right}"
    }
  }

}
