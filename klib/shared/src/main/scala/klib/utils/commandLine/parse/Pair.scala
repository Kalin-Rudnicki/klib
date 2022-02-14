package klib.utils.commandLine.parse

import scala.annotation.tailrec

import klib.utils.{*, given}

opaque type Pair[+L, +R] = (L, R)
extension [L, R](pair: Pair[L, R]) {
  def left: L = pair._1
  def right: R = pair._2
}
object Pair {

  def apply[L, R](left: L, right: R): Pair[L, R] = (left, right)

  def zipPair(helpConfig: HelpConfig, pair: Pair[List[LeftLine], List[String]]): List[Pair[ColorString, String]] = {
    @tailrec
    def loop(
        leftLines: List[LeftLine],
        rightLines: List[String],
        stack: List[Pair[ColorString, String]],
    ): List[Pair[ColorString, String]] =
      leftLines match {
        case leftHead :: leftTail =>
          val leftCS = leftHead.toColorString(helpConfig.leftPadding)
          if (leftCS.length > helpConfig.maxParamsWidth)
            loop(
              leftTail,
              rightLines,
              (leftCS, "") :: stack,
            )
          else
            rightLines match {
              case rightHead :: rightTail =>
                loop(
                  leftTail,
                  rightTail,
                  (leftCS, rightHead) :: stack,
                )
              case Nil =>
                loop(
                  leftTail,
                  rightLines,
                  (leftCS, "") :: stack,
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

  def zipPairs(helpConfig: HelpConfig)(pairs: List[Pair[List[LeftLine], List[String]]]*): List[Pair[ColorString, String]] =
    pairs.toList.flatMap(_.flatMap(zipPair(helpConfig, _)))

  def makeLines(helpConfig: HelpConfig, pairs: List[Pair[ColorString, String]]): List[String] = {
    val maxParamsUsed: Int = pairs.map(_.left.length).maxOption.getOrElse(0).min(helpConfig.maxParamsWidth)

    pairs.map { pair =>
      s"${" " * helpConfig.leftPadding}${pair.left}${" " * (maxParamsUsed - pair.left.length + helpConfig.centerPadding)}${pair.right}"
    }
  }

}
