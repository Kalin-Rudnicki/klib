package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object TestExec {

  val flagMap: Map[String, InfiniteSet[String]] =
    Map(
      // Tier 1
      "i-1" -> InfiniteSet.Inclusive("f-1", "f-2"),
      "i-2" -> InfiniteSet.Inclusive("f-3", "f-4"),
      "e-1" -> InfiniteSet.Exclusive("f-1", "f-2"),
      "e-2" -> InfiniteSet.Exclusive("f-3", "f-4"),
      // Tier 2
      "i-i-1" -> InfiniteSet.Inclusive("i-1"),
      "i-e-1" -> InfiniteSet.Inclusive("e-1"),
      "e-i-1" -> InfiniteSet.Exclusive("i-1"),
      "e-e-1" -> InfiniteSet.Exclusive("e-1"),
      // Tier 3
      "i-i-i-1" -> InfiniteSet.Inclusive("i-i-1"),
      "i-i-e-1" -> InfiniteSet.Inclusive("i-e-1"),
      "i-e-i-1" -> InfiniteSet.Inclusive("e-i-1"),
      "i-e-e-1" -> InfiniteSet.Inclusive("e-e-1"),
      "e-i-i-1" -> InfiniteSet.Exclusive("i-i-1"),
      "e-i-e-1" -> InfiniteSet.Exclusive("i-e-1"),
      "e-e-i-1" -> InfiniteSet.Exclusive("e-i-1"),
      "e-e-e-1" -> InfiniteSet.Exclusive("e-e-1"),
    )

  val extraFlags: Set[String] = Set("f-5", "f-6")

  val explicit: Set[String] =
    flagMap.toList.flatMap { case (key, values) => key :: values.explicit.toList }.toSet | extraFlags

  val executable: Executable =
    Executable { (logger, _) =>
      for {
        _ <- logger.log(
          L(
            L.log.info("[Start]"),
            explicit.toList.sorted.map { f =>
              L.requireFlags(f)(L.log.detailed(f))
            },
            L.log.info("[End]"),
          ),
        )
      } yield ()
    }.mapLogger {
      _.withFlagMap(flagMap)
    }

  def main(args: Array[String]): Unit =
    executable
      .apply(args)
      .runSyncOrExit(None)

}
