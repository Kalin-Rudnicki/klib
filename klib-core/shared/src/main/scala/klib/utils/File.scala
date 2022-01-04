package klib.utils

import java.awt.image.BufferedImage
import java.io.{File => JavaFile}
import java.io.PrintWriter
import java.io.RandomAccessFile
import java.net.URI
import java.net.URL
import javax.imageio.ImageIO

import scala.io.Source

import klib.Implicits._
import klib.fp.types._

final class File private (val wrapped: JavaFile) {

  // --- Read ---

  def readString: IO[String] =
    IO(Source.fromFile(wrapped)).bracket(_.mkString.pure[IO])(_.close.pure[IO])

  def readBytes: IO[Array[Byte]] =
    IO(new RandomAccessFile(wrapped, "r")).bracket { raf =>
      for {
        len <- raf.length.pure[IO]
        bb = new Array[Byte](len.toInt) // WARNING (KR) : Could be an issue...
        _ <- raf.readFully(bb).pure[IO]
      } yield bb
    }(_.close().pure[IO])

  def readMImage: IO[Maybe[BufferedImage]] =
    ImageIO.read(wrapped).pure[IO].map(Maybe(_))

  def readImage: IO[BufferedImage] =
    readMImage.flatMap {
      case Some(img) => img.pure[IO]
      case None      => IO.errorMessage(s"Not an image: $wrapped")
    }

  // --- Write ---

  def writeString(contents: String): IO[Unit] =
    new PrintWriter(wrapped).pure[IO].bracket(_.write(contents).pure[IO])(_.close().pure[IO])

  def writeFileBytes(bytes: Array[Byte]): IO[Unit] =
    IO(new RandomAccessFile(wrapped, "rw")).bracket { _.write(bytes).pure[IO] }(_.close().pure[IO])

  def writeImage(formatName: String, image: BufferedImage): IO[Boolean] =
    ImageIO.write(image, formatName, wrapped).pure[IO]

  // --- Utils ---

  override def toString: String = wrapped.toString

  def exists: IO[Boolean] =
    wrapped.exists.pure[IO]

  def ensureExists: IO[Unit] =
    exists.flatMap {
      case true  => ().pure[IO]
      case false => IO.errorMessage(s"File does not exist: $wrapped")
    }

  def ensureDoesntExist: IO[Unit] =
    exists.flatMap {
      case false => ().pure[IO]
      case true  => IO.errorMessage(s"File already exists: $wrapped")
    }

  def delete: IO[Boolean] =
    wrapped.delete.pure[IO]

  def mkdir: IO[Boolean] =
    wrapped.mkdir.pure[IO]

  def mkdirs: IO[Boolean] =
    wrapped.mkdirs.pure[IO]

  def isFile: IO[Boolean] =
    wrapped.isFile.pure[IO]

  def isDirectory: IO[Boolean] =
    wrapped.isFile.pure[IO]

  def isHidden: IO[Boolean] =
    wrapped.isHidden.pure[IO]

  def parent: Maybe[File] =
    Maybe(wrapped.getParentFile).map(File.fromJavaFile)

  def parentOrSelf: File =
    parent.getOrElse(this)

  def child(ps: NonEmptyList[String]): File =
    File.fromJavaFile(new JavaFile(wrapped, ps.toList.mkString("/")))

  def child(p0: String, pN: String*): File =
    child(NonEmptyList(p0, pN.toList))

  def uri: IO[URI] =
    wrapped.toURI.pure[IO]

  def url: IO[URL] =
    uri.flatMap(_.toURL.pure[IO])

  def absolute: File =
    File.fromJavaFile(wrapped.getAbsoluteFile)

  def canonical: File =
    File.fromJavaFile(wrapped.getCanonicalFile)

  def path: String =
    wrapped.getPath

  def name: String =
    wrapped.getName

  def baseName: String = {
    val name = this.name
    val index = name.lastIndexOf('.')
    if (index <= 0) name
    else name.substring(0, index)
  }

  def extName: Maybe[String] = {
    val name = this.name
    val index = name.lastIndexOf('.')
    if (index <= 0) None
    else name.substring(index + 1).some
  }

}
object File {

  def fromPath(path: String): File =
    fromJavaFile(new JavaFile(path))

  def fromJavaFile(file: JavaFile): File =
    new File(file)

  def userHome: IO[File] =
    System.getProperty("user.home").pure[IO].map(fromPath)

}
