package klib

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object TestExec {

  /*
  final class Conf(args: Seq[String]) extends Executable.Conf(args) {
    version("This is the version")
    this.appendDefaultToDescription = true
    val opt1: ScallopOption[String] = opt(descr = "", default = "opt1".someOpt)
    val opt2: ScallopOption[String] = opt(descr = "| Line #1", default = "opt2".someOpt)
    val opt3: ScallopOption[String] = opt(descr = "| Line #1\n| Line #2", default = "opt3".someOpt)
    val opt4: ScallopOption[String] = opt()

    verify()
  }
  object Conf extends Executable.ConfBuilder(new Conf(_))
   */

  val executable: Executable =
    Executable { (_logger, _) =>
      val logger =
        _logger
          .withMappedFlag("flags", "flag-1", "flag-2")
          .withMappedFlag("flags", "flag-3")
          .withMappedFlag("flag-3", "flag-4")
          .withMappedFlag("flag-4", "flags")

      for {
        _ <- logger.log.indent.by(1)
        _ <- logger.log.detailed("Test!")
        _ <- logger.log.indent.by(2)
        _ <- logger.log(
          L(
            L.log.detailed("Test-2"),
            L.log.detailed("Test-3"),
          ),
        )
        _ <- logger.log.indent.to(0)
        _ <- logger.log(
          L(
            L.requireFlags("flag-1")(
              L.log.detailed("flag-1"),
            ),
            L.requireFlags("flag-2")(
              L.log.detailed("flag-2"),
            ),
            L.requireFlags("flag-1", "flag-2")(
              L.log.detailed("flag-1 & flag-2"),
            ),
          ),
        )
        _ <- IO.error(Message("Oops...")): IO[Unit]
      } yield ()
    }

  def main(args: Array[String]): Unit =
    executable
      .apply(args)
      .runSyncOrExit(None)

}
