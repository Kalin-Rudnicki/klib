package klib.utils

import zio._

extension (lco: Logger.type) {

  def live(
      logTolerance: Logger.LogLevel with Logger.LogLevel.Tolerance,
      defaultIndent: String = "    ",
      flags: Set[String] = Set.empty,
      flagMap: Map[String, InfiniteSet[String]] = Map.empty,
      colorMode: Logger.ColorMode = Logger.ColorMode.Extended,
      initialIndents: List[String] = Nil,
      extraSources: List[UIO[Logger.Source]] = Nil,
  ): ZLayer[Any, Nothing, Logger] =
    Logger.layer(
      defaultIndent = defaultIndent,
      flags = flags,
      flagMap = flagMap,
      colorMode = colorMode,
      initialIndents = initialIndents,
      sources = Logger.Source.stdOut(logTolerance) :: extraSources,
    )

}
