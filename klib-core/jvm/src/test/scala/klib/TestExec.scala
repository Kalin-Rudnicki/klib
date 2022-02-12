package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object TestExec {

  val executable: Executable =
    Executable { (logger, _) =>
      for {
        _ <- run(File.fromPath("a/b/c").canonical, 5)
        _ <- logger.log.break()
        _ <- run(File.fromPath("/a/b/c").canonical, 5)
        _ <- logger.log.break()
        _ <- run(File.fromPath("/a.b.c/d.e.f/.g").canonical, 5)
      } yield ()
    }

  def main(args: Array[String]): Unit =
    executable
      .apply(args)
      .runSyncOrExit(None)

}
