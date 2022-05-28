package klib.utils

import scala.jdk.CollectionConverters.*
import zio.*

trait FileSystem {

  def createFileObject(path: String): KTask[File]

  def roots: KTask[Array[File]]

}
object FileSystem {

  def fromJavaFileSystem(fs: java.nio.file.FileSystem): FileSystem =
    new FileSystem {

      override def createFileObject(path: String): KTask[File] =
        ZIO.kAttempt(s"Unable to create file for path : $path")(File.fromNIOPath(fs.getPath(path)))

      override def roots: KTask[Array[File]] =
        ZIO.kAttempt("Unable to get filesystem roots")(fs.getRootDirectories.asScala.toArray.map(File.fromNIOPath))

    }

  def layer(fileSystem: => java.nio.file.FileSystem): KTaskLayer[FileSystem] =
    ZIO.kAttempt("Unable to create filesystem")(fileSystem).map(FileSystem.fromJavaFileSystem).toLayer

  def live: KTaskLayer[FileSystem] =
    layer(java.nio.file.FileSystems.getDefault)

  // =====| API |=====

  def roots: KRIO[FileSystem, Array[File]] =
    ZIO.service[FileSystem].flatMap(_.roots)

}
