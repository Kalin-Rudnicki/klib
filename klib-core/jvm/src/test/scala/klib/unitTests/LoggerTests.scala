package klib.unitTests

import org.scalatest.funspec.AnyFunSpec

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils._
import klib.utils._

final class LoggerTests extends AnyFunSpec {

  describe("expandFlags") {

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

    val expMap: Map[String, InfiniteSet[String]] =
      Map(
        // Tier 0
        "f-1" -> InfiniteSet.Inclusive("f-1"),
        "f-2" -> InfiniteSet.Inclusive("f-2"),
        "f-3" -> InfiniteSet.Inclusive("f-3"),
        "f-4" -> InfiniteSet.Inclusive("f-4"),
        "f-5" -> InfiniteSet.Inclusive("f-5"),
        "f-6" -> InfiniteSet.Inclusive("f-6"),
        // Tier 1
        "i-1" -> InfiniteSet.Inclusive("i-1", "f-1", "f-2"),
        "i-2" -> InfiniteSet.Inclusive("i-2", "f-3", "f-4"),
        "e-1" -> InfiniteSet.Exclusive("f-1", "f-2"),
        "e-2" -> InfiniteSet.Exclusive("f-3", "f-4"),
        // Tier 2
        "i-i-1" -> InfiniteSet.Inclusive("i-i-1", "i-1", "f-1", "f-2"),
        "i-e-1" -> InfiniteSet.Exclusive("f-1", "f-2"),
        "e-i-1" -> InfiniteSet.Exclusive("i-1", "f-1", "f-2"),
        "e-e-1" -> InfiniteSet.Exclusive(),
        // Tier 3
        "i-i-i-1" -> InfiniteSet.Inclusive("i-i-i-1", "i-i-1", "i-1", "f-1", "f-2"),
        "i-i-e-1" -> InfiniteSet.Exclusive("f-1", "f-2"),
        "i-e-i-1" -> InfiniteSet.Exclusive("i-1", "f-1", "f-2"),
        "i-e-e-1" -> InfiniteSet.Exclusive(),
        "e-i-i-1" -> InfiniteSet.Exclusive("i-i-1", "i-1", "f-1", "f-2"),
        "e-i-e-1" -> InfiniteSet.Exclusive("i-e-1"),
        "e-e-i-1" -> InfiniteSet.Exclusive(),
        "e-e-e-1" -> InfiniteSet.Exclusive(),
      )

    val explicit: Set[String] =
      flagMap.toList.flatMap { case (key, values) => key :: values.explicit.toList }.toSet | extraFlags

    describe("single-input") {
      explicit
        .groupBy { flag =>
          if (flag.contains('f')) 0
          else flag.count(_ == '-')
        }
        .toList
        .foreach {
          case (tier, flags) =>
            describe(s"Tier-$tier") {
              flags.toList.sorted.foreach { inputFlag =>
                it(inputFlag) {
                  expMap.get(inputFlag).toMaybe match {
                    case Some(exp) => assertResult(exp)(Logger.expandFlags(Set(inputFlag), flagMap))
                    case None      => fail(s"Missing expected value for flag: $inputFlag")
                  }
                }
              }
            }
        }
    }

    describe("combined-input") {
      def permOf(depth: Int): Set[Set[String]] =
        if (depth <= 0)
          Set(Set())
        else {
          permOf(depth - 1).flatMap { flags =>
            explicit.map { f =>
              flags + f
            }
          }

        }

      2.to(3)
        .foreach { d =>
          describe(s"depth: $d") {
            permOf(d).foreach { flags =>
              it(flags.toList.sorted.mkString("flags(", ", ", ")")) {
                type H[T] = ErrorAccumulator[String, T]

                {
                  flags.toList
                    .map(f => expMap.get(f).toMaybe.toEA(f)): List[H[InfiniteSet[String]]]
                }.traverse match {
                  case Alive(exps)   => assertResult(InfiniteSet.union(exps: _*))(Logger.expandFlags(flags, flagMap))
                  case Dead(missing) => fail(s"Missing expected value for flag(s): ${missing.mkString(", ")}")
                }
              }
            }
          }
        }
    }

  }

}
