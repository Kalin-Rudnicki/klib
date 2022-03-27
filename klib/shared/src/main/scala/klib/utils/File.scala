package klib.utils

export OpaqueFile.File
import cats.syntax.option.*
import io.circe.*
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

object OpaqueFile {

  opaque type File = Path
  extension (self: File) {

    def toPath: Path = self
    def toJavaFile: JavaFile = self.toFile

    def createFile(fileAttributes: FileAttribute[_]*): TaskM[Unit] =
      ZIOM.attempt(Files.createFile(self, fileAttributes*))
    def createDirectory(fileAttributes: FileAttribute[_]*): TaskM[Unit] =
      ZIOM.attempt(Files.createDirectory(self, fileAttributes*))
    def createDirectories(fileAttributes: FileAttribute[_]*): TaskM[Unit] =
      ZIOM.attempt(Files.createDirectories(self, fileAttributes*))

    def createSymbolicLink(target: File, fileAttributes: FileAttribute[_]*): TaskM[Unit] =
      ZIOM.attempt(Files.createSymbolicLink(self, target, fileAttributes*))
    def createLink(existing: File): TaskM[Unit] =
      ZIOM.attempt(Files.createLink(self, existing))

    def delete: TaskM[Boolean] =
      ZIOM.attempt(Files.deleteIfExists(self))

    def copyTo(target: File, copyOptions: CopyOption*): TaskM[Unit] =
      ZIOM.attempt(Files.copy(self, target, copyOptions*))
    def moveTo(target: File, copyOptions: CopyOption*): TaskM[Unit] =
      ZIOM.attempt(Files.move(self, target, copyOptions*))

    def existsWrapped: TaskM[Boolean] =
      ZIOM.attempt(Files.exists(self))

    def exists: TaskM[Boolean] =
      ZIOM.attempt(Files.exists(self))
    def isFile: TaskM[Boolean] =
      ZIOM.attempt(Files.isRegularFile(self))
    def isDirectory: TaskM[Boolean] =
      ZIOM.attempt(Files.isDirectory(self))
    def isSymbolicLink: TaskM[Boolean] =
      ZIOM.attempt(Files.isSymbolicLink(self))

    def outputStream(openOptions: OpenOption*): TaskM[OutputStream] =
      ZIOM.attempt(Files.newOutputStream(self, openOptions*))
    def inputStream(openOptions: OpenOption*): TaskM[InputStream] =
      ZIOM.attempt(Files.newInputStream(self, openOptions*))
    def bufferedWriter(openOptions: OpenOption*): TaskM[BufferedWriter] =
      ZIOM.attempt(Files.newBufferedWriter(self, openOptions*))
    def bufferedReader: TaskM[BufferedReader] =
      ZIOM.attempt(Files.newBufferedReader(self))

    def getLastModifiedTime: TaskM[Long] =
      ZIOM.attempt(Files.getLastModifiedTime(self)).map(_.toMillis)
    def setLastModifiedTime(millis: Long): TaskM[Unit] =
      ZIOM.attempt(Files.setLastModifiedTime(self, FileTime.fromMillis(millis)))

    def size: TaskM[Long] =
      ZIOM.attempt(Files.size(self))

    def child(path: String): TaskM[File] =
      ZIOM.attempt(self.resolve(path))

    def children: TaskM[Array[File]] =
      ZIOM.attempt(Files.list(self)).map(_.iterator().asScala.toArray.map(File.fromNIOPath))

    def fileName: File.Name = {
      val name = self.getFileName.toString
      name.lastIndexOf('.') match {
        case -1  => File.Name(name, name, None)
        case idx => File.Name(name, name.substring(0, idx), name.substring(idx + 1).some)
      }
    }

    // =====|  |=====

    private def bracket[R, C <: AutoCloseable, A](
        acquire: => RIOM[R, C],
        use: C => RIOM[R, A],
    ): RIOM[R, A] =
      ZIO
        .acquireReleaseWith[R, KError[Nothing], C](acquire)
        .apply(s => ZIOM.attempt(s.close()).orDieKlib)
        .apply(use)
    def withOutputStream[R, A](use: OutputStream => RIOM[R, A]): RIOM[R, A] =
      bracket(outputStream(), use)
    def withInputStream[R, A](use: InputStream => RIOM[R, A]): RIOM[R, A] =
      bracket(inputStream(), use)
    def withBufferedWriter[R, A](use: BufferedWriter => RIOM[R, A]): RIOM[R, A] =
      bracket(bufferedWriter(), use)
    def withBufferedReader[R, A](use: BufferedReader => RIOM[R, A]): RIOM[R, A] =
      bracket(bufferedReader, use)

    def writeBytes(bytes: Array[Byte]): TaskM[Unit] =
      withOutputStream(s => ZIOM.attempt(s.write(bytes)))
    def writeString(string: String): TaskM[Unit] =
      withOutputStream(s => ZIOM.attempt(s.write(string.getBytes)))
    def writeJson(json: Json, toString: Json => String = _.toString): TaskM[Unit] =
      writeString(toString(json))
    def writeEncodedJson[T: Encoder](t: T, toString: Json => String = _.toString): TaskM[Unit] =
      writeJson(Encoder[T].apply(t), toString)

    def readBytes: TaskM[Array[Byte]] =
      withInputStream(s => ZIOM.attempt(s.readAllBytes()))
    def readString: TaskM[String] =
      withInputStream(s => ZIOM.attempt(new String(s.readAllBytes())))
    def readJson: TaskM[Json] =
      readString.flatMap(s => ZIO.fromEither(parser.parse(s)).toKlibError)
    def readDecodedJson[T: Decoder]: TaskM[T] =
      readJson.flatMap(j => ZIO.fromEither(Decoder[T].decodeJson(j)).toKlibError)

    def ensureExists: TaskM[Unit] =
      existsWrapped.flatMap {
        case true  => ZIO.unit
        case false => ZIO.fail(KError.message.unexpected("File does not exist"))
      }

    def createIfDNE: TaskM[Unit] =
      existsWrapped.flatMap {
        case true  => ZIO.unit
        case false => createFile()
      }

  }
  object File {

    def fromPath(path: String): RIOM[FileSystem, File] =
      ZIO.service[FileSystem].flatMap(_.createFileObject(path))

    def fromNIOPath(path: Path): File = path

    def homeDirectory: RIOM[FileSystem, File] =
      ZIOM.attempt(java.lang.System.getProperty("user.home")).flatMap(fromPath)

    final case class Name(
        name: String,
        base: String,
        ext: Option[String],
    )

  }

}
