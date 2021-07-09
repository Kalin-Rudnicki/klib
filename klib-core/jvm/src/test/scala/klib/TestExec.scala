package klib

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.utils.Logger.{helpers => L}

object TestExec {

  final class Conf(args: Seq[String]) extends Executable.Conf(args) {
    version("This is the version")

    verify()
  }
  object Conf extends Executable.ConfBuilder(new Conf(_))

  val executable: Executable =
    Executable.fromConf(Conf) { (logger, conf) =>
      for {
        _ <- logger(L.log.info("Testing!"))
      } yield ()
    }

  def main(args: Array[String]): Unit =
    executable
      .apply(args)
      .runSyncOrExit(None)

}
