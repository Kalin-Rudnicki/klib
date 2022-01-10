package klib.utils

import scala.jdk.CollectionConverters._

import zio.*

trait FileSystem {

  def createFileObject(path: String): Task[File]

  def roots: Task[Array[File]]

}
object FileSystem {

  def fromJavaFileSystem(fs: java.nio.file.FileSystem): FileSystem =
    new FileSystem {

      override def createFileObject(path: String): Task[File] =
        ZIO.attempt(new File(fs.getPath(path)))

      override def roots: Task[Array[File]] =
        ZIO.attempt(fs.getRootDirectories.asScala.toArray.map(File(_)))

    }

  def layer(fileSystem: => java.nio.file.FileSystem): ZLayer[Any, Throwable, FileSystem] =
    ZLayer.fromZIO(ZIO.attempt(fileSystem).map(FileSystem.fromJavaFileSystem))

  def live: ZLayer[Any, Throwable, FileSystem] =
    layer(java.nio.file.FileSystems.getDefault)

  // =====| API |=====

  def roots: RIO[FileSystem, Array[File]] =
    ZIO.service[FileSystem].flatMap(_.roots)

}
