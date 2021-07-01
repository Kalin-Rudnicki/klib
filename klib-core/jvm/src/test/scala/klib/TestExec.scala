package klib

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._
import klib.utils._, Logger.{helpers => L}

object TestExec extends Executable.ExecFromConf {

  final class Conf(args: Seq[String]) extends ScallopConf(args) {
    verify()
  }

  override def buildConf(args: Seq[String]): Conf = new Conf(args)

  override def run(logger: Logger, conf: Conf): IO[Unit] = {

    for {
      _ <- logger(
        L(
          L.log.info("Yay!!!"),
          L.log.info("Line 1\nLine 2"),
          L.break(),
          L.log.info("..."),
          L.break(false),
          L.log.info("..."),
        ),
      )
    } yield ()
  }

  def main(args: Array[String]): Unit =
    this.apply(args)

}
