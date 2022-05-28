package klib.utils

export OpaqueFile.File
import cats.syntax.option.*
import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File as JavaFile
import java.io.InputStream
import java.io.OutputStream
import java.lang.AutoCloseable
import java.nio.file.CopyOption
import java.nio.file.Files
import java.nio.file.OpenOption
import java.nio.file.Path
import java.nio.file.attribute.FileAttribute
import java.nio.file.attribute.FileTime
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import zio.*
import zio.json.*
import zio.json.ast.Json

object OpaqueFile {

  opaque type File = Path
  extension (self: File) {

    def toPath: Path = self
    def toJavaFile: JavaFile = self.toFile

    def createFile(fileAttributes: FileAttribute[_]*): KTask[Unit] =
      ZIO.kAttempt(s"Unable to create file : $self", KError.ErrorType.SystemFailure)(Files.createFile(self, fileAttributes*))
    def createDirectory(fileAttributes: FileAttribute[_]*): KTask[Unit] =
      ZIO.kAttempt(s"Unable to create directory : $self", KError.ErrorType.SystemFailure)(Files.createDirectory(self, fileAttributes*))
    def createDirectories(fileAttributes: FileAttribute[_]*): KTask[Unit] =
      ZIO.kAttempt(s"Unable to create directories : $self", KError.ErrorType.SystemFailure)(Files.createDirectories(self, fileAttributes*))

    def createSymbolicLink(target: File, fileAttributes: FileAttribute[_]*): KTask[Unit] =
      ZIO.kAttempt(s"Unable to create symbolic link : $self", KError.ErrorType.SystemFailure)(Files.createSymbolicLink(self, target, fileAttributes*))
    def createLink(existing: File): KTask[Unit] =
      ZIO.kAttempt(s"Unable to create link : $self", KError.ErrorType.SystemFailure)(Files.createLink(self, existing))

    def delete: KTask[Boolean] =
      ZIO.kAttempt(s"Unable to delete file : $self", KError.ErrorType.SystemFailure)(Files.deleteIfExists(self))

    def copyTo(target: File, copyOptions: CopyOption*): KTask[Unit] =
      ZIO.kAttempt(s"Unable to copy file : $self", KError.ErrorType.SystemFailure)(Files.copy(self, target, copyOptions*))
    def moveTo(target: File, copyOptions: CopyOption*): KTask[Unit] =
      ZIO.kAttempt(s"Unable to move file : $self", KError.ErrorType.SystemFailure)(Files.move(self, target, copyOptions*))

    def existsWrapped: KTask[Boolean] =
      ZIO.kAttempt(s"Unable to check if file exists : $self", KError.ErrorType.SystemFailure)(Files.exists(self))

    def exists: KTask[Boolean] =
      ZIO.kAttempt(s"Unable to check if file exists : $self", KError.ErrorType.SystemFailure)(Files.exists(self))
    def isFile: KTask[Boolean] =
      ZIO.kAttempt(s"Unable to check if file is a file : $self", KError.ErrorType.SystemFailure)(Files.isRegularFile(self))
    def isDirectory: KTask[Boolean] =
      ZIO.kAttempt(s"Unable to check if file is a directory : $self", KError.ErrorType.SystemFailure)(Files.isDirectory(self))
    def isSymbolicLink: KTask[Boolean] =
      ZIO.kAttempt(s"Unable to check if file is a symbolic link : $self", KError.ErrorType.SystemFailure)(Files.isSymbolicLink(self))

    // TODO (KR) : I think these should probably be scoped
    def outputStream(openOptions: OpenOption*): KRIO[Scope, OutputStream] =
      ZIO.acquireReleaseClosable(ZIO.kAttempt(s"Unable to create OutputStream for file : $self", KError.ErrorType.SystemFailure)(Files.newOutputStream(self, openOptions*)))
    def inputStream(openOptions: OpenOption*): KRIO[Scope, InputStream] =
      ZIO.acquireReleaseClosable(ZIO.kAttempt(s"Unable to create InputStream for file : $self", KError.ErrorType.SystemFailure)(Files.newInputStream(self, openOptions*)))
    def bufferedWriter(openOptions: OpenOption*): KRIO[Scope, BufferedWriter] =
      ZIO.acquireReleaseClosable(ZIO.kAttempt(s"Unable to create BufferedWriter for file : $self", KError.ErrorType.SystemFailure)(Files.newBufferedWriter(self, openOptions*)))
    def bufferedReader: KRIO[Scope, BufferedReader] =
      ZIO.acquireReleaseClosable(ZIO.kAttempt(s"Unable to create BufferedReader for file : $self", KError.ErrorType.SystemFailure)(Files.newBufferedReader(self)))

    def getLastModifiedTime: KTask[Long] =
      ZIO.kAttempt(s"Unable to get last modified time for file : $self", KError.ErrorType.SystemFailure)(Files.getLastModifiedTime(self)).map(_.toMillis)
    def setLastModifiedTime(millis: Long): KTask[Unit] =
      ZIO.kAttempt(s"Unable to set last modified time for file : $self", KError.ErrorType.SystemFailure)(Files.setLastModifiedTime(self, FileTime.fromMillis(millis)))

    def size: KTask[Long] =
      ZIO.kAttempt(s"Unable to get size of file : $self", KError.ErrorType.SystemFailure)(Files.size(self))

    def child(path: String): KTask[File] =
      ZIO.kAttempt(s"Unable to create child instance of file : $self ($path)", KError.ErrorType.SystemFailure)(self.resolve(path))

    def children: KTask[Array[File]] =
      ZIO.kAttempt(s"Unable to get children for file : $self", KError.ErrorType.SystemFailure)(Files.list(self)).map(_.iterator().asScala.toArray.map(File.fromNIOPath))

    def fileName: File.Name = {
      val name = self.getFileName.toString
      name.lastIndexOf('.') match {
        case -1  => File.Name(name, name, None)
        case idx => File.Name(name, name.substring(0, idx), name.substring(idx + 1).some)
      }
    }

    // =====|  |=====

    def writeBytes(bytes: Array[Byte]): KTask[Unit] =
      ZIO.scoped(outputStream().flatMap(s => ZIO.kAttempt(s"Unable to write bytes to file : $self", KError.ErrorType.SystemFailure)(s.write(bytes))))
    def writeString(string: String): KTask[Unit] =
      ZIO.scoped(outputStream().flatMap(s => ZIO.kAttempt(s"Unable to write string to file : $self", KError.ErrorType.SystemFailure)(s.write(string.getBytes))))
    def writeJson[T: JsonEncoder](t: T, spaces: Option[Int] = None): KTask[Unit] =
      writeString(JsonEncoder[T].encodeJson(t, spaces).toString)

    def readBytes: KTask[Array[Byte]] =
      ZIO.scoped(inputStream().flatMap(s => ZIO.kAttempt(s"Unable to read bytes from file : $self", KError.ErrorType.SystemFailure)(s.readAllBytes())))
    def readString: KTask[String] =
      ZIO.scoped(inputStream().flatMap(s => ZIO.kAttempt(s"Unable to read bytes from file : $self", KError.ErrorType.SystemFailure)(String(s.readAllBytes()))))
    def readDecodedJson[T: JsonDecoder]: KTask[T] =
      readString.flatMap { s => ZIO.fromEitherKError(JsonDecoder[T].decodeJson(s))(m => KError.UserError(s"Unable to decode json from file : $self\n$m")) }

    def ensureExists: KTask[Unit] =
      existsWrapped.flatMap {
        case true  => ZIO.unit
        case false => ZIO.failNEL(KError.UserError(s"File that should exist does not : $self"))
      }

    def createIfDNE: KTask[Unit] =
      existsWrapped.flatMap {
        case true  => ZIO.unit
        case false => createFile()
      }

  }
  object File {

    def fromPath(path: String): KRIO[FileSystem, File] =
      ZIO.service[FileSystem].flatMap(_.createFileObject(path))

    def fromNIOPath(path: Path): File = path

    def homeDirectory: KRIO[FileSystem, File] =
      ZIO.kAttempt("Unable to get user.home from system properties")(java.lang.System.getProperty("user.home")).flatMap(fromPath)

    final case class Name(
        name: String,
        base: String,
        ext: Option[String],
    )

  }

}
