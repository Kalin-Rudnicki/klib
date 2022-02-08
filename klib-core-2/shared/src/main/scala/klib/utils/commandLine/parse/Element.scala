package klib.utils.commandLine.parse

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.list.*

final case class Element(
    baseName: String,
    typeName: String,
    primaryParams: NonEmptyList[Param],
    aliasParams: List[Param],
    allParams: NonEmptyList[Param],
    requirementLevel: Option[RequirementLevel],
    description: List[String],
) {

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

    def extraDescriptions: List[String] =
      List(
        requirementLevel.map(_.toString),
      ).flatten
    def paramString(params: NonEmptyList[Param]): String = params.toList.map(_.formattedName).mkString(", ")

    if (helpConfig.helpExtra)
      loop(
        List(
          paramString(primaryParams).some,
          aliasParams.toNel.map(params => s"${" " * helpConfig.leftPadding}${paramString(params)}"),
        ).flatten,
        description ::: extraDescriptions,
        Nil,
      )
    else
      loop(
        paramString(primaryParams) :: Nil,
        description,
        Nil,
      )
  }

}
