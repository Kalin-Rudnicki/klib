package klib

import org.rogach.scallop._
import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  object Ex1 extends Executable.ExecFromConf {

    final class Conf(args: Seq[String]) extends ScallopConf(args) {
      val a = opt[String](required = true)
      val b = opt[String](default = "b".someOpt)

      verify()
    }

    override def buildConf(args: Seq[String]): Conf = new Conf(args)

    override def run(logger: Logger, conf: Conf): ??[Unit] =
      for {
        _ <- logger() { src =>
          src.info(s"a: ${conf.a()}")
          src.info(s"b: ${conf.b()}")
        }.wrap
      } yield ()

  }

  Executable
    .fromSubCommands(
      "ex-1" -> Ex1,
    )(args)
    .runSync

}
