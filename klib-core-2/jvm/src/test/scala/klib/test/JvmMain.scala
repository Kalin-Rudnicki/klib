package klib.test

import cats.syntax.option.*
import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import zio.*

import klib.utils.*

object JvmMain extends ZIOApp {

  override type Environment = ZEnv with Logger

  override def layer: ZLayer[ZIOAppArgs, Any, Environment] =
    ZEnv.live ++
      Logger.live(Logger.LogLevel.Detailed)

  override implicit def tag: Tag[Environment] = Tag[Environment]

  override def run: RIO[Environment, Any] =
    for {
      _ <- Logger.println("=====| Shared Main |=====")
      (roots, homeDirectory) <- FileSystem.live.toRuntime(RuntimeConfig.global).use {
        _.run(FileSystem.roots <*> File.homeDirectory)
      }
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
    } yield ()

}
