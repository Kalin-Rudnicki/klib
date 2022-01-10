package klib.utils

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.InputStream
import java.io.OutputStream
import java.nio.file.CopyOption
import java.nio.file.Files
import java.nio.file.FileSystems
import java.nio.file.OpenOption
import java.nio.file.Path
import java.nio.file.attribute.FileAttribute
import java.nio.file.attribute.FileTime

import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

import zio.*

trait FileSystem {

  def createFileObject(path: String): Task[File]

  // TODO (KR) : Do these all need to be part of the service?

  def createFile(file: File, fileAttributes: List[FileAttribute[_]]): Task[Unit]
  def createDirectory(file: File, fileAttributes: List[FileAttribute[_]]): Task[Unit]
  def createDirectories(file: File, fileAttributes: List[FileAttribute[_]]): Task[Unit]

  def createSymbolicLink(link: File, target: File, fileAttributes: List[FileAttribute[_]]): Task[Unit]
  def createLink(link: File, existing: File): Task[Unit]

  def delete(file: File): Task[Boolean]

  def copy(source: File, target: File, copyOptions: List[CopyOption]): Task[Unit]
  def move(source: File, target: File, copyOptions: List[CopyOption]): Task[Unit]

  def exists(file: File): Task[Boolean]
  def isFile(file: File): Task[Boolean]
  def isDirectory(file: File): Task[Boolean]
  def isSymbolicLink(file: File): Task[Boolean]

  def outputStream(file: File, openOptions: List[OpenOption]): Task[OutputStream]
  def inputStream(file: File, openOptions: List[OpenOption]): Task[InputStream]
  def bufferedWriter(file: File, openOptions: List[OpenOption]): Task[BufferedWriter]
  def bufferedReader(file: File): Task[BufferedReader]

  def getLastModifiedTime(file: File): Task[Long]
  def setLastModifiedTime(file: File, millis: Long): Task[Unit]

  def size(file: File): Task[Long]

  def children(file: File): Task[Array[File]]
  def walkTree[R, E >: Throwable, A: ClassTag](file: File, withFile: File => ZIO[R, E, A]): ZIO[R, E, Array[A]]

}
object FileSystem {

  def fromJavaFileSystem(fs: java.nio.file.FileSystem): FileSystem =
    new FileSystem {

      override def createFileObject(path: String): Task[File] =
        ZIO.attempt(new File(fs.getPath(path)))

      override def createFile(file: File, fileAttributes: List[FileAttribute[_]]): Task[Unit] =
        ZIO.attempt(Files.createFile(file.path, fileAttributes: _*))
      override def createDirectory(file: File, fileAttributes: List[FileAttribute[_]]): Task[Unit] =
        ZIO.attempt(Files.createDirectory(file.path, fileAttributes: _*))
      override def createDirectories(file: File, fileAttributes: List[FileAttribute[_]]): Task[Unit] =
        ZIO.attempt(Files.createDirectories(file.path, fileAttributes: _*))

      override def createSymbolicLink(link: File, target: File, fileAttributes: List[FileAttribute[_]]): Task[Unit] =
        ZIO.attempt(Files.createSymbolicLink(link.path, target.path, fileAttributes: _*))
      override def createLink(link: File, existing: File): Task[Unit] =
        ZIO.attempt(Files.createLink(link.path, existing.path))

      override def delete(file: File): Task[Boolean] =
        ZIO.attempt(Files.deleteIfExists(file.path))

      override def copy(source: File, target: File, copyOptions: List[CopyOption]): Task[Unit] =
        ZIO.attempt(Files.copy(source.path, target.path, copyOptions: _*))
      override def move(source: File, target: File, copyOptions: List[CopyOption]): Task[Unit] =
        ZIO.attempt(Files.move(source.path, target.path, copyOptions: _*))

      override def exists(file: File): Task[Boolean] =
        ZIO.attempt(Files.exists(file.path))
      override def isFile(file: File): Task[Boolean] =
        ZIO.attempt(Files.isRegularFile(file.path))
      override def isDirectory(file: File): Task[Boolean] =
        ZIO.attempt(Files.isDirectory(file.path))
      override def isSymbolicLink(file: File): Task[Boolean] =
        ZIO.attempt(Files.isSymbolicLink(file.path))

      override def outputStream(file: File, openOptions: List[OpenOption]): Task[OutputStream] =
        ZIO.attempt(Files.newOutputStream(file.path, openOptions: _*))
      override def inputStream(file: File, openOptions: List[OpenOption]): Task[InputStream] =
        ZIO.attempt(Files.newInputStream(file.path, openOptions: _*))
      override def bufferedWriter(file: File, openOptions: List[OpenOption]): Task[BufferedWriter] =
        ZIO.attempt(Files.newBufferedWriter(file.path, openOptions: _*))
      override def bufferedReader(file: File): Task[BufferedReader] =
        ZIO.attempt(Files.newBufferedReader(file.path))

      override def getLastModifiedTime(file: File): Task[Long] =
        ZIO.attempt(Files.getLastModifiedTime(file.path)).map(_.toMillis)
      override def setLastModifiedTime(file: File, millis: Long): Task[Unit] =
        ZIO.attempt(Files.setLastModifiedTime(file.path, FileTime.fromMillis(millis)))

      override def size(file: File): Task[Long] =
        ZIO.attempt(Files.size(file.path))

      override def children(file: File): Task[Array[File]] =
        ZIO.attempt(Files.list(file.path)).map(_.iterator().asScala.toArray.map(File(_)))
      override def walkTree[R, E >: Throwable, A: ClassTag](file: File, withFile: File => ZIO[R, E, A]): ZIO[R, E, Array[A]] =
        for {
          stream <- ZIO.attempt(Files.walk(file.path))
          as <- ZIO.foreach(stream.iterator().asScala.map(File(_)).toArray)(withFile)
        } yield as

    }

  def defaultJavaFileSystem: FileSystem =
    fromJavaFileSystem(FileSystems.getDefault)

  def layer(fileSystem: => java.nio.file.FileSystem): ZLayer[Any, Throwable, FileSystem] =
    ZLayer.fromZIO(ZIO.attempt(fileSystem).map(FileSystem.fromJavaFileSystem))

  def live: ZLayer[Any, Throwable, FileSystem] =
    layer(FileSystems.getDefault)

}
