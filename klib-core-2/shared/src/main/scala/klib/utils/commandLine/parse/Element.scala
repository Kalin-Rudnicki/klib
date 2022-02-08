package klib.utils.commandLine.parse

import cats.data.NonEmptyList

final case class Element(
    baseName: String,
    typeName: String,
    params: NonEmptyList[Param],
    requirementLevel: RequirementLevel,
    description: List[String],
)
