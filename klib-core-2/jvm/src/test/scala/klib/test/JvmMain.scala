package klib.test

import cats.syntax.option.*
import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import zio.*

import klib.utils.*

object JvmMain extends ZIOApp {

  private val fileSystemLayer: ZLayer[Any, Throwable, FileSystem] =
    FileSystem.currentPlatformJIMFS

  private val logFile: RIO[FileSystem, File] =
    File.fromPath("log-file.txt")

  private val loggerLayer: ZLayer[FileSystem, Throwable, Logger] = {
    for {
      logFile <- logFile
      logger <- Logger(
        defaultIndent = "    ",
        flags = Set.empty,
        flagMap = Map.empty,
        sources = List(
          Logger.Source.stdOut(Logger.LogLevel.Info),
          Logger.Source.file(logFile, Logger.LogLevel.Debug),
        ),
        colorMode = Logger.ColorMode.Extended,
        initialIndents = Nil,
      )
    } yield logger
  }.toLayer

  override type Environment = ZEnv & Logger & FileSystem

  override def layer: ZLayer[ZIOAppArgs, Any, Environment] =
    ZEnv.live ++
      fileSystemLayer >+>
      loggerLayer

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
      _ <- Logger.execute.all(
        Logger.LogLevel.All.map(ll => Logger.println.event(ll.displayName).requireLogLevel(ll)),
      )
      logFile <- logFile
      _ <- zio.Console.printLine("log-file contents:")
      fc <- logFile.readString
      _ <- zio.Console.printLine(fc)
    } yield ()

}
