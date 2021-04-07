package klib

import java.io.File
import java.io.PrintWriter

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  def getFileName(fileNum: Int): File =
    new File(s"test-$fileNum.txt")

  def getFileContents(fileNum: Int): String =
    s"=====| Header |=====\nThis is File #$fileNum\nAdded stuff...\n"

  def adHocWrite(fileNum: Int): Unit = {
    val pw = new PrintWriter(getFileName(fileNum))
    pw.print(getFileContents(fileNum))
    pw.close()
  }

  def ioWrite(fileNum: Int): IO[Unit] =
    IO.writeFile(getFileName(fileNum), getFileContents(fileNum))

  adHocWrite(1)
  adHocWrite(2).pure[IO].runSync
  ioWrite(3).runSync

}
