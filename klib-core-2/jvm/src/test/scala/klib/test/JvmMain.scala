package klib.test

import cats.syntax.option.*
import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import zio.*

import klib.utils.*

object JvmMain extends ZIOApp {

  override type Environment = ZEnv with Logger with FileSystem

  override def layer: ZLayer[ZIOAppArgs, Any, Environment] =
    ZEnv.live ++
      Logger.live(Logger.LogLevel.Detailed) ++
      FileSystem.live
  // FileSystem.layer(Jimfs.newFileSystem(Configuration.forCurrentPlatform()))

  override implicit def tag: Tag[Environment] = Tag[Environment]

  override def run: RIO[Environment, Any] =
    for {
      _ <- Logger.println("=====| Shared Main |=====")
      file <- File.fromPath("/home/kalin/dev/current/klib/klib-core-2/shared/src/main/scala/klib/utils")
      _ <- file.walk { file =>
        file.isFile.flatMap {
          case true =>
            Logger.execute(
              Logger.println.info.event(file),
              Logger.println.info.event(file.path.toAbsolutePath),
              Logger.break.event(),
            )
          case false =>
            Task.unit
        }
      }
    } yield ()

}
