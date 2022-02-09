package klib.utils.commandLine.parse

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.list.*

sealed trait Element {
  def helpStringLinesPair(helpConfig: HelpConfig, indentCount: Int): Pair[List[String]]
  def allParams: NonEmptyList[Param]
}
object Element {

  private def baseIndentString(helpConfig: HelpConfig, indentCount: Int): String =
    " " * (helpConfig.leftPadding * indentCount)

  private def indentString(helpConfig: HelpConfig): String =
    " " * helpConfig.leftPadding

  final case class ParamElement(
      baseName: String,
      typeName: String,
      primaryParams: NonEmptyList[Param],
      aliasParams: List[Param],
      allParams: NonEmptyList[Param],
      requirementLevel: Option[RequirementLevel],
      description: List[String],
  ) extends Element {

    override def helpStringLinesPair(helpConfig: HelpConfig, indentCount: Int): Pair[List[String]] = {
      def extraDescriptions: List[String] =
        List(
          requirementLevel.map(_.toString),
        ).flatten
      def paramString(params: NonEmptyList[Param]): String = params.toList.map(_.formattedName).mkString(", ")

      if (helpConfig.helpExtra)
        Pair(
          List(
            paramString(primaryParams).some,
            aliasParams.toNel.map(params => s"${" " * helpConfig.leftPadding}${paramString(params)}"),
          ).flatten,
          description ::: extraDescriptions,
        )
      else
        Pair(
          paramString(primaryParams) :: Nil,
          description,
        )
    }

  }

}
