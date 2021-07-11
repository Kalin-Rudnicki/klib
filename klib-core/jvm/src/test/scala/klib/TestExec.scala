package klib

import org.rogach.scallop._

import klib.predefs.common.{given, _}
import klib.predefs.jvm.{given, _}

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
