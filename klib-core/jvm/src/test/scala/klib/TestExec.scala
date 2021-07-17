package klib

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.utils.Logger.{helpers => L}

object TestExec {

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

  val executable: Executable =
    Executable.fromConf(Conf) { (logger, conf) =>
      for {
        _ <- logger(L.log.info("Testing!"))
        myVar = Var.`null`[String]
        _ <- logger(L.log.info(myVar))
        assign1 = myVar.value = "Assign1"
        _ <- logger(L.log.info(myVar))
        _ <- assign1
        _ <- logger(L.log.info(myVar))
        _ <- myVar.value = "Assign2"
        _ <- logger(L.log.info(myVar))
      } yield ()
    }

  def main(args: Array[String]): Unit =
    executable
      .apply(args)
      .runSyncOrExit(None)

}
