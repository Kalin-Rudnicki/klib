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
        ZIOM.attempt(File.fromNIOPath(fs.getPath(path)))

      override def roots: TaskM[Array[File]] =
        ZIOM.attempt(fs.getRootDirectories.asScala.toArray.map(File.fromNIOPath))

    }

  def layer(fileSystem: => java.nio.file.FileSystem): TaskLayerM[FileSystem] =
    ZIOM.attempt(fileSystem).map(FileSystem.fromJavaFileSystem).toLayer

  def live: TaskLayerM[FileSystem] =
    layer(java.nio.file.FileSystems.getDefault)

  // =====| API |=====

  def roots: RIOM[FileSystem, Array[File]] =
    ZIO.service[FileSystem].flatMap(_.roots)

}
