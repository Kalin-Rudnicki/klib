package klib.utils

import java.awt.image.BufferedImage
import java.io.File
import java.io.PrintWriter
import java.io.RandomAccessFile
import javax.imageio.ImageIO

import scala.io.Source

import klib.Implicits._
import klib.fp.types._

object FileUtils {

  // --- Read ---

  def readFile(path: File): IO[String] =
    IO(Source.fromFile(path)).bracket(_.mkString.pure[IO])(_.close.pure[IO])

  def readFileBytes(path: File): IO[Array[Byte]] =
    IO(new RandomAccessFile(path, "r")).bracket { raf =>
      for {
        len <- raf.length.pure[IO]
        bb = new Array[Byte](len.toInt) // WARNING (KR) : Could be an issue...
        _ <- raf.readFully(bb).pure[IO]
      } yield bb
    }(_.close().pure[IO])

  def readImage(path: File): IO[BufferedImage] =
    ImageIO.read(path).pure[IO]

  // --- Write ---

  def writeFile(path: File, contents: String): IO[Unit] =
    new PrintWriter(path).pure[IO].bracket(_.write(contents).pure[IO])(_.close().pure[IO])

  def writeFileBytes(path: File, bytes: Array[Byte]): IO[Unit] =
    IO(new RandomAccessFile(path, "rw")).bracket { _.write(bytes).pure[IO] }(_.close().pure[IO])

  def writeImage(path: File, formatName: String, image: BufferedImage): IO[Boolean] =
    ImageIO.write(image, formatName, path).pure[IO]

}
