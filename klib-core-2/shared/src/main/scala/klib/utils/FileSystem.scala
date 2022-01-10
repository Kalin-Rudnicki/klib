package klib.utils

import zio.*

trait FileSystem {

  def createFileObject(path: String): Task[File]

}
object FileSystem {

  def fromJavaFileSystem(fs: java.nio.file.FileSystem): FileSystem =
    path => ZIO.attempt(new File(fs.getPath(path)))

  def layer(fileSystem: => java.nio.file.FileSystem): ZLayer[Any, Throwable, FileSystem] =
    ZLayer.fromZIO(ZIO.attempt(fileSystem).map(FileSystem.fromJavaFileSystem))

  def live: ZLayer[Any, Throwable, FileSystem] =
    layer(java.nio.file.FileSystems.getDefault)

}
