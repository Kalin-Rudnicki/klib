package klib.utils

import java.io.BufferedReader
import java.io.BufferedWriter
import java.lang.AutoCloseable
import java.io.InputStream
import java.io.OutputStream
import java.nio.file.CopyOption
import java.nio.file.OpenOption
import java.nio.file.Path
import java.nio.file.attribute.FileAttribute

import scala.reflect.ClassTag

import zio.*

final class File(val path: Path) {

  def createFile(fileAttributes: FileAttribute[_]*): RIO[FileSystem, Unit] =
    ZIO.service[FileSystem].flatMap(_.createFile(this, fileAttributes.toList))
  def createDirectory(fileAttributes: FileAttribute[_]*): RIO[FileSystem, Unit] =
    ZIO.service[FileSystem].flatMap(_.createDirectory(this, fileAttributes.toList))
  def createDirectories(fileAttributes: FileAttribute[_]*): RIO[FileSystem, Unit] =
    ZIO.service[FileSystem].flatMap(_.createDirectories(this, fileAttributes.toList))

  def createSymbolicLink(target: File, fileAttributes: FileAttribute[_]*): RIO[FileSystem, Unit] =
    ZIO.service[FileSystem].flatMap(_.createSymbolicLink(this, target, fileAttributes.toList))
  def createLink(existing: File): RIO[FileSystem, Unit] =
    ZIO.service[FileSystem].flatMap(_.createLink(this, existing))

  def delete: RIO[FileSystem, Boolean] =
    ZIO.service[FileSystem].flatMap(_.delete(this))

  def copyTo(target: File, copyOptions: CopyOption*): RIO[FileSystem, Unit] =
    ZIO.service[FileSystem].flatMap(_.copy(this, target, copyOptions.toList))
  def moveTo(target: File, copyOptions: CopyOption*): RIO[FileSystem, Unit] =
    ZIO.service[FileSystem].flatMap(_.move(this, target, copyOptions.toList))

  def exists: RIO[FileSystem, Boolean] =
    ZIO.service[FileSystem].flatMap(_.exists(this))
  def isFile: RIO[FileSystem, Boolean] =
    ZIO.service[FileSystem].flatMap(_.isFile(this))
  def isDirectory: RIO[FileSystem, Boolean] =
    ZIO.service[FileSystem].flatMap(_.isDirectory(this))
  def isSymbolicLink: RIO[FileSystem, Boolean] =
    ZIO.service[FileSystem].flatMap(_.isSymbolicLink(this))

  def outputStream(openOptions: OpenOption*): RIO[FileSystem, OutputStream] =
    ZIO.service[FileSystem].flatMap(_.outputStream(this, openOptions.toList))
  def inputStream(openOptions: OpenOption*): RIO[FileSystem, InputStream] =
    ZIO.service[FileSystem].flatMap(_.inputStream(this, openOptions.toList))
  def bufferedWriter(openOptions: OpenOption*): RIO[FileSystem, BufferedWriter] =
    ZIO.service[FileSystem].flatMap(_.bufferedWriter(this, openOptions.toList))
  def bufferedReader: RIO[FileSystem, BufferedReader] =
    ZIO.service[FileSystem].flatMap(_.bufferedReader(this))

  def getLastModifiedTime: RIO[FileSystem, Long] =
    ZIO.service[FileSystem].flatMap(_.getLastModifiedTime(this))
  def setLastModifiedTime(millis: Long): RIO[FileSystem, Unit] =
    ZIO.service[FileSystem].flatMap(_.setLastModifiedTime(this, millis))

  def size: RIO[FileSystem, Long] =
    ZIO.service[FileSystem].flatMap(_.size(this))

  def children: RIO[FileSystem, Array[File]] =
    ZIO.service[FileSystem].flatMap(_.children(this))
  def walk[R, E >: Throwable, A: ClassTag](withFile: File => ZIO[R, E, A]): ZIO[FileSystem with R, E, Array[A]] =
    ZIO.service[FileSystem].flatMap(_.walkTree(this, withFile))

  // =====|  |=====

  private def bracket[R, E >: Throwable, C <: AutoCloseable, A](
      acquire: => ZIO[FileSystem, Throwable, C],
      use: C => ZIO[R, E, A],
  ): ZIO[FileSystem with R, E, A] =
    new ZIO.Acquire[FileSystem, E, C](() => acquire)
      .apply(s => ZIO.attempt(s.close()).orDie)
      .apply(use)
  def withOutputStream[R, E >: Throwable, A](use: OutputStream => ZIO[R, E, A]): ZIO[FileSystem with R, E, A] =
    bracket(outputStream(), use)
  def withInputStream[R, E >: Throwable, A](use: InputStream => ZIO[R, E, A]): ZIO[FileSystem with R, E, A] =
    bracket(inputStream(), use)
  def withBufferedWriter[R, E >: Throwable, A](use: BufferedWriter => ZIO[R, E, A]): ZIO[FileSystem with R, E, A] =
    bracket(bufferedWriter(), use)
  def withBufferedReader[R, E >: Throwable, A](use: BufferedReader => ZIO[R, E, A]): ZIO[FileSystem with R, E, A] =
    bracket(bufferedReader, use)

  def writeBytes(bytes: Array[Byte]): RIO[FileSystem, Unit] =
    withOutputStream(s => ZIO.attempt(s.write(bytes)))
  def writeString(string: String): RIO[FileSystem, Unit] =
    withOutputStream(s => ZIO.attempt(s.write(string.getBytes)))

  def readBytes: RIO[FileSystem, Array[Byte]] =
    withInputStream(s => ZIO.attempt(s.readAllBytes()))
  def readString: RIO[FileSystem, String] =
    withInputStream(s => ZIO.attempt(new String(s.readAllBytes())))

  // =====|  |=====

  def ensureExists: RIO[FileSystem, Unit] =
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

}
