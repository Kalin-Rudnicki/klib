package klib.utils.commandLine.parse

import scala.annotation.tailrec

import cats.data.NonEmptyList

final case class Element(
    baseName: String,
    typeName: String,
    params: NonEmptyList[Param],
    requirementLevel: RequirementLevel,
    description: List[String],
) {

  private def extraDescription: List[String] =
    List(
      requirementLevel.toString,
    )

  private def fullDescription(helpExtra: Boolean): List[String] =
    if (helpExtra) description ::: extraDescription
    else description

  // TODO (KR) : Improve
  def toHelpString(helpConfig: HelpConfig): List[(String, String)] = {
    @tailrec
    def loop(
        leftLines: List[String],
        rightLines: List[String],
        stack: List[(String, String)],
    ): List[(String, String)] =
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
      params.toList.map(_.formattedName).mkString(", ") :: Nil,
      fullDescription(helpConfig.helpExtra),
      Nil,
    )
  }

}
