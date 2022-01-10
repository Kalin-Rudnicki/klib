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
      // FileSystem.layer(FileSystem.defaultJavaFileSystem)
      FileSystem.layer(Jimfs.newFileSystem(Configuration.forCurrentPlatform()))

  override implicit def tag: Tag[Environment] = Tag[Environment]

  override def run: RIO[Environment, Any] =
    for {
      _ <- Logger.println("=====| Shared Main |=====")
      file <- File.fromPath(".")
      _ <- Logger.println(file)
      _ <- Logger.println(file.path)
      _ <- Logger.println(file.path.toUri)
      _ <- Logger.println(file.path.toAbsolutePath)
      children <- file.children
      _ <- Logger.println(children.toList)
    } yield ()

}
