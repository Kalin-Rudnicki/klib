package klib.utils

import java.io.BufferedReader
import java.io.BufferedWriter
import java.lang.AutoCloseable
import java.io.InputStream
import java.io.OutputStream
import java.nio.file.CopyOption
import java.nio.file.Files
import java.nio.file.OpenOption
import java.nio.file.Path
import java.nio.file.attribute.FileAttribute
import java.nio.file.attribute.FileTime

import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

import zio.*

final class File(val path: Path) {

  def createFile(fileAttributes: FileAttribute[_]*): Task[Unit] =
    ZIO.attempt(Files.createFile(this.path, fileAttributes: _*))
  def createDirectory(fileAttributes: FileAttribute[_]*): Task[Unit] =
    ZIO.attempt(Files.createDirectory(this.path, fileAttributes: _*))
  def createDirectories(fileAttributes: FileAttribute[_]*): Task[Unit] =
    ZIO.attempt(Files.createDirectories(this.path, fileAttributes: _*))

  def createSymbolicLink(target: File, fileAttributes: FileAttribute[_]*): Task[Unit] =
    ZIO.attempt(Files.createSymbolicLink(this.path, target.path, fileAttributes: _*))
  def createLink(existing: File): Task[Unit] =
    ZIO.attempt(Files.createLink(this.path, existing.path))

  def delete: Task[Boolean] =
    ZIO.attempt(Files.deleteIfExists(this.path))

  def copyTo(target: File, copyOptions: CopyOption*): Task[Unit] =
    ZIO.attempt(Files.copy(this.path, target.path, copyOptions: _*))
  def moveTo(target: File, copyOptions: CopyOption*): Task[Unit] =
    ZIO.attempt(Files.move(this.path, target.path, copyOptions: _*))

  def exists: Task[Boolean] =
    ZIO.attempt(Files.exists(this.path))
  def isFile: Task[Boolean] =
    ZIO.attempt(Files.isRegularFile(this.path))
  def isDirectory: Task[Boolean] =
    ZIO.attempt(Files.isDirectory(this.path))
  def isSymbolicLink: Task[Boolean] =
    ZIO.attempt(Files.isSymbolicLink(this.path))

  def outputStream(openOptions: OpenOption*): Task[OutputStream] =
    ZIO.attempt(Files.newOutputStream(this.path, openOptions: _*))
  def inputStream(openOptions: OpenOption*): Task[InputStream] =
    ZIO.attempt(Files.newInputStream(this.path, openOptions: _*))
  def bufferedWriter(openOptions: OpenOption*): Task[BufferedWriter] =
    ZIO.attempt(Files.newBufferedWriter(this.path, openOptions: _*))
  def bufferedReader: Task[BufferedReader] =
    ZIO.attempt(Files.newBufferedReader(this.path))

  def getLastModifiedTime: Task[Long] =
    ZIO.attempt(Files.getLastModifiedTime(this.path)).map(_.toMillis)
  def setLastModifiedTime(millis: Long): Task[Unit] =
    ZIO.attempt(Files.setLastModifiedTime(this.path, FileTime.fromMillis(millis)))

  def size: Task[Long] =
    ZIO.attempt(Files.size(this.path))

  def children: Task[Array[File]] =
    ZIO.attempt(Files.list(this.path)).map(_.iterator().asScala.toArray.map(File(_)))
  def walk[R, E >: Throwable, A: ClassTag](withFile: File => ZIO[R, E, A]): ZIO[R, E, Array[A]] =
    for {
      stream <- ZIO.attempt(Files.walk(this.path))
      as <- ZIO.foreach(stream.iterator().asScala.map(File(_)).toArray)(withFile)
    } yield as

  // =====|  |=====

  private def bracket[R, E >: Throwable, C <: AutoCloseable, A](
      acquire: => Task[C],
      use: C => ZIO[R, E, A],
  ): ZIO[R, E, A] =
    ZIO
      .acquireReleaseWith[R, E, C](acquire)
      .apply(s => ZIO.attempt(s.close()).orDie)
      .apply(use)
  def withOutputStream[R, E >: Throwable, A](use: OutputStream => ZIO[R, E, A]): ZIO[R, E, A] =
    bracket(outputStream(), use)
  def withInputStream[R, E >: Throwable, A](use: InputStream => ZIO[R, E, A]): ZIO[R, E, A] =
    bracket(inputStream(), use)
  def withBufferedWriter[R, E >: Throwable, A](use: BufferedWriter => ZIO[R, E, A]): ZIO[R, E, A] =
    bracket(bufferedWriter(), use)
  def withBufferedReader[R, E >: Throwable, A](use: BufferedReader => ZIO[R, E, A]): ZIO[R, E, A] =
    bracket(bufferedReader, use)

  def writeBytes(bytes: Array[Byte]): Task[Unit] =
    withOutputStream(s => ZIO.attempt(s.write(bytes)))
  def writeString(string: String): Task[Unit] =
    withOutputStream(s => ZIO.attempt(s.write(string.getBytes)))

  def readBytes: Task[Array[Byte]] =
    withInputStream(s => ZIO.attempt(s.readAllBytes()))
  def readString: Task[String] =
    withInputStream(s => ZIO.attempt(new String(s.readAllBytes())))

  def ensureExists: Task[Unit] =
    exists.flatMap {
      case true  => ZIO.unit
      case false => ZIO.fail(new RuntimeException(s"File ($path) does not exist"))
    }

  // =====|  |=====

  override def toString: String = path.toString

}
object File {

  def fromPath(path: String): RIO[FileSystem, File] =
    ZIO.service[FileSystem].flatMap(_.createFileObject(path))

  def homeDirectory: RIO[FileSystem, File] =
    ZIO.attempt(java.lang.System.getProperty("user.home")).flatMap(fromPath)

}
