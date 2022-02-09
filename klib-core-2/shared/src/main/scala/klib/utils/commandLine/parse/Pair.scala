package klib.utils.commandLine.parse

import scala.annotation.tailrec

opaque type Pair[+T] = (T, T)
extension [T](pair: Pair[T]) {
  def left: T = pair._1
  def right: T = pair._2
}
object Pair {

  def apply[T](left: T, right: T): Pair[T] = (left, right)

  def zipPair(helpConfig: HelpConfig, pair: Pair[List[String]]): List[Pair[String]] = {
    @tailrec
    def loop(
        leftLines: List[String],
        rightLines: List[String],
        stack: List[(String, String)],
    ): List[Pair[String]] =
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

  def zipPairs(helpConfig: HelpConfig)(pairs: List[Pair[List[String]]]*): List[Pair[String]] =
    pairs.toList.flatMap(_.flatMap(zipPair(helpConfig, _)))

  def makeLines(helpConfig: HelpConfig, pairs: List[Pair[String]]): List[String] = {
    val maxParamsUsed: Int = pairs.map(_.left.length).maxOption.getOrElse(0).min(helpConfig.maxParamsWidth)

    pairs.map { pair =>
      s"${" " * helpConfig.leftPadding}${pair.left}${" " * (maxParamsUsed - pair.left.length + helpConfig.centerPadding)}${pair.right}"
    }
  }

}
