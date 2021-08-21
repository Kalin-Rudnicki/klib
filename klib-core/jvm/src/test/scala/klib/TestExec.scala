package klib

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

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
    Executable { (logger, _) =>
      for {
        _ <- logger.log.detailed("Sleeping...")
        _ <- IO { Thread.sleep(2500) }.toAsyncIO.toIO(Duration(1, "s").some)
        _ <- logger.log.detailed("Done!")
      } yield ()
    }

  def main(args: Array[String]): Unit =
    executable
      .apply(args)
      .runSyncOrExit(None)

}
