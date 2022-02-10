package klib.utils.commandLine.parse

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.list.*

import klib.utils.{*, given}

sealed trait Element {
  def helpStringLinesPair(helpConfig: HelpConfig): Pair[List[LeftLine], List[String]]
  def allParams: List[Param]
}
object Element {

  final case class ParamElement(
      baseName: String,
      typeName: String,
      primaryParams: NonEmptyList[Param],
      aliasParams: List[Param],
      requirementLevel: Option[RequirementLevel],
      description: List[String],
  ) extends Element {

    override def helpStringLinesPair(helpConfig: HelpConfig): Pair[List[LeftLine], List[String]] = {
      def extraDescriptions: List[String] =
        List(
          requirementLevel.map(_.toString),
        ).flatten
      def paramString(params: NonEmptyList[Param], extraIndent: Boolean): ColorString =
        params.toList
          .map(_.formattedName)
          .csMkString(", ")

      if (helpConfig.helpExtra)
        Pair(
          List(
            LeftLine(paramString(primaryParams, false)).some,
            aliasParams.toNel.map(p => LeftLine(paramString(p, true)).indentBy(1)),
          ).flatten,
          description ::: extraDescriptions,
        )
      else
        Pair(
          LeftLine(paramString(primaryParams, false)) :: Nil,
          description,
        )
    }

    override def allParams: List[Param] = primaryParams.toList ::: aliasParams
  }

  case object Break extends Element {
    override def helpStringLinesPair(helpConfig: HelpConfig): Pair[List[LeftLine], List[String]] =
      Pair(LeftLine("") :: Nil, Nil)
    override def allParams: List[Param] = Nil
  }

}
