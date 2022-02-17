package klib.utils

import scala.jdk.CollectionConverters.*

import zio.*

trait FileSystem {

  def createFileObject(path: String): TaskM[File]

  def roots: TaskM[Array[File]]

}
object FileSystem {

  def fromJavaFileSystem(fs: java.nio.file.FileSystem): FileSystem =
    new FileSystem {

      override def createFileObject(path: String): TaskM[File] =
        ZIO.attemptM(File.fromNIOPath(fs.getPath(path)))

      override def roots: TaskM[Array[File]] =
        ZIO.attemptM(fs.getRootDirectories.asScala.toArray.map(File.fromNIOPath))

    }

  def layer(fileSystem: => java.nio.file.FileSystem): ZLayer[Any, Message, FileSystem] =
    ZIO.attemptM(fileSystem).map(FileSystem.fromJavaFileSystem).toLayer

  def live: ZLayer[Any, Message, FileSystem] =
    layer(java.nio.file.FileSystems.getDefault)

  // =====| API |=====

  def roots: ZIO[FileSystem, Message, Array[File]] =
    ZIO.service[FileSystem].flatMap(_.roots)

}
