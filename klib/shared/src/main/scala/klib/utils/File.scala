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
      ZIO.attemptM(Files.createFile(self, fileAttributes*))
    def createDirectory(fileAttributes: FileAttribute[_]*): TaskM[Unit] =
      ZIO.attemptM(Files.createDirectory(self, fileAttributes*))
    def createDirectories(fileAttributes: FileAttribute[_]*): TaskM[Unit] =
      ZIO.attemptM(Files.createDirectories(self, fileAttributes*))

    def createSymbolicLink(target: File, fileAttributes: FileAttribute[_]*): TaskM[Unit] =
      ZIO.attemptM(Files.createSymbolicLink(self, target, fileAttributes*))
    def createLink(existing: File): TaskM[Unit] =
      ZIO.attemptM(Files.createLink(self, existing))

    def delete: TaskM[Boolean] =
      ZIO.attemptM(Files.deleteIfExists(self))

    def copyTo(target: File, copyOptions: CopyOption*): TaskM[Unit] =
      ZIO.attemptM(Files.copy(self, target, copyOptions*))
    def moveTo(target: File, copyOptions: CopyOption*): TaskM[Unit] =
      ZIO.attemptM(Files.move(self, target, copyOptions*))

    def existsWrapped: TaskM[Boolean] =
      ZIO.attemptM(Files.exists(self))

    def exists: TaskM[Boolean] =
      ZIO.attemptM(Files.exists(self))
    def isFile: TaskM[Boolean] =
      ZIO.attemptM(Files.isRegularFile(self))
    def isDirectory: TaskM[Boolean] =
      ZIO.attemptM(Files.isDirectory(self))
    def isSymbolicLink: TaskM[Boolean] =
      ZIO.attemptM(Files.isSymbolicLink(self))

    def outputStream(openOptions: OpenOption*): TaskM[OutputStream] =
      ZIO.attemptM(Files.newOutputStream(self, openOptions*))
    def inputStream(openOptions: OpenOption*): TaskM[InputStream] =
      ZIO.attemptM(Files.newInputStream(self, openOptions*))
    def bufferedWriter(openOptions: OpenOption*): TaskM[BufferedWriter] =
      ZIO.attemptM(Files.newBufferedWriter(self, openOptions*))
    def bufferedReader: TaskM[BufferedReader] =
      ZIO.attemptM(Files.newBufferedReader(self))

    def getLastModifiedTime: TaskM[Long] =
      ZIO.attemptM(Files.getLastModifiedTime(self)).map(_.toMillis)
    def setLastModifiedTime(millis: Long): TaskM[Unit] =
      ZIO.attemptM(Files.setLastModifiedTime(self, FileTime.fromMillis(millis)))

    def size: TaskM[Long] =
      ZIO.attemptM(Files.size(self))

    def child(path: String): TaskM[File] =
      ZIO.attemptM(self.resolve(path))

    def children: TaskM[Array[File]] =
      ZIO.attemptM(Files.list(self)).map(_.iterator().asScala.toArray.map(File.fromNIOPath))
    def walk[R, A: ClassTag](withFile: File => ZIO[R, Message, A]): ZIO[R, Message, Array[A]] =
      for {
        stream <- ZIO.attemptM(Files.walk(self))
        as <- ZIO.foreach(stream.iterator().asScala.map(File.fromNIOPath).toArray)(withFile)
      } yield as

    def fileName: File.Name = {
      val name = self.getFileName.toString
      name.lastIndexOf('.') match {
        case -1  => File.Name(name, name, None)
        case idx => File.Name(name, name.substring(0, idx), name.substring(idx + 1).some)
      }
    }

    // =====|  |=====

    private def bracket[R, C <: AutoCloseable, A](
        acquire: => TaskM[C],
        use: C => ZIO[R, Message, A],
    ): ZIO[R, Message, A] =
      ZIO
        .acquireReleaseWith[R, Message, C](acquire)
        .apply(s => ZIO.attemptM(s.close()).orDieWith(_ => new RuntimeException)) // TODO (KR) : ...
        .apply(use)
    def withOutputStream[R, A](use: OutputStream => ZIO[R, Message, A]): ZIO[R, Message, A] =
      bracket(outputStream(), use)
    def withInputStream[R, A](use: InputStream => ZIO[R, Message, A]): ZIO[R, Message, A] =
      bracket(inputStream(), use)
    def withBufferedWriter[R, A](use: BufferedWriter => ZIO[R, Message, A]): ZIO[R, Message, A] =
      bracket(bufferedWriter(), use)
    def withBufferedReader[R, A](use: BufferedReader => ZIO[R, Message, A]): ZIO[R, Message, A] =
      bracket(bufferedReader, use)

    def writeBytes(bytes: Array[Byte]): TaskM[Unit] =
      withOutputStream(s => ZIO.attemptM(s.write(bytes)))
    def writeString(string: String): TaskM[Unit] =
      withOutputStream(s => ZIO.attemptM(s.write(string.getBytes)))
    def writeJson(json: Json, toString: Json => String = _.toString): TaskM[Unit] =
      writeString(toString(json))
    def writeEncodedJson[T: Encoder](t: T, toString: Json => String = _.toString): TaskM[Unit] =
      writeJson(Encoder[T].apply(t), toString)

    def readBytes: TaskM[Array[Byte]] =
      withInputStream(s => ZIO.attemptM(s.readAllBytes()))
    def readString: TaskM[String] =
      withInputStream(s => ZIO.attemptM(new String(s.readAllBytes())))
    def readJson: TaskM[Json] =
      readString.flatMap(s => ZIO.fromEither(parser.parse(s)).messageError)
    def readDecodedJson[T: Decoder]: TaskM[T] =
      readJson.flatMap(j => ZIO.fromEither(Decoder[T].decodeJson(j)).messageError)

    def ensureExists: TaskM[Unit] =
      existsWrapped.flatMap {
        case true  => ZIO.unit
        case false => ZIO.fail(Message.same(s"File ($self) does not exist"))
      }

    def createIfDNE: TaskM[Unit] =
      existsWrapped.flatMap {
        case true  => ZIO.unit
        case false => createFile()
      }

  }
  object File {

    def fromPath(path: String): ZIO[FileSystem, Message, File] =
      ZIO.service[FileSystem].flatMap(_.createFileObject(path))

    def fromNIOPath(path: Path): File = path

    def homeDirectory: ZIO[FileSystem, Message, File] =
      ZIO.attemptM(java.lang.System.getProperty("user.home")).flatMap(fromPath)

    final case class Name(
        name: String,
        base: String,
        ext: Option[String],
    )

  }

}
