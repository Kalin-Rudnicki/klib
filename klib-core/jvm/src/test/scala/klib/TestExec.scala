package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object TestExec {

  val executable: Executable =
    Executable { (logger, _) =>
      def run(file: File, times: Int): IO[Unit] =
        if (times > 0)
          for {
            _ <- logger.log.info((file, file.name, file.baseName, file.extName))
            _ <- file.parent match {
              case Some(parent) => run(parent, times - 1)
              case None         => logger.log.info("No Parent")
            }
          } yield ()
        else ().pure[IO]

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
