package klib.test

import cats.syntax.option.*
import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import zio.*

import klib.utils.*

object JvmMain extends ZIOApp {

  override type Environment = ZEnv & Logger

  override def layer: ZLayer[ZIOAppArgs, Any, Environment] =
    ZEnv.live ++
      Logger.live(Logger.LogLevel.Detailed)

  override implicit def tag: Tag[Environment] = Tag[Environment]

  // REMOVE : ...
  private def runtimeEx: Unit = {
    val tmp1: ZLayer[Any, Throwable, FileSystem] = FileSystem.live
    val tmp2: Managed[Throwable, Runtime[FileSystem]] = tmp1.toRuntime(RuntimeConfig.global)
    val tmp3: ZIO[Any, Throwable, File] = tmp2.use(_.run(File.homeDirectory))

    val tmp4: ZIO[Any, Throwable, Runtime[FileSystem]] = tmp2.useNow

    val tmp5: URIO[Int, Runtime[Int]] = ZIO.runtime[Int]

    for {
      runtime <- tmp5
      res = runtime.unsafeRunSync(ZIO.succeed(5)).toEither
    } yield ()

    ()
  }

  private def test: Unit = {
    val zio1: ZIO[Any, Nothing, Unit] = ???
    val zio2: ZIO[Logger, Nothing, Int] = ???
    val zio3: ZIO[FileSystem, String, String] = ???

    val zio4: ZIO[Logger & FileSystem, String, (Int, String)] = zio1 <*> zio2 <*> zio3
    /*
    val zio5: ZIO[FileSystem, String, (Int, String)] =
      zio4.provideSomeLayer[FileSystem].apply[String, FileSystem](FileSystem.live.orDie)
     */

    ()
  }

  override def run: RIO[Environment, Any] =
    for {
      _ <- Logger.println("=====| Shared Main |=====")
      (roots, homeDirectory) <-
        (FileSystem.roots <*> File.homeDirectory <*> Logger.println("Test")).provideSomeLayer(FileSystem.live)
      _ <- ZIO.foreach(roots) { file =>
        Logger.println.info(file) *>
          Logger
            .withIndent(1) {
              file.children >>= (ZIO.foreach(_)(Logger.println.info(_)))
            }
            .unit
      }
      _ <- Logger.break()
      _ <- homeDirectory.exists >>= (Logger.println.info(_))
      runtime <- ZIO.runtime[Environment]
      res1 = runtime.unsafeRunSync(ZIO.succeed(5)).toEither
      res2 = runtime.unsafeRunSync(ZIO.fail(new RuntimeException("Fail....."))).toEither
      _ <- Logger.println.info((res1, res2))
    } yield ()

}
