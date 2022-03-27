package klib.web

import zio.*

import klib.utils.*

// TODO (KR) : Need some sort of typeClass for this, so that it can be handled differently on different platforms
final class FileRequest[FileT] {

  def forwardToFile(file: FileT): TaskM[Unit] = ???

  def fileLengthLargerThanInt: UIO[Boolean] = ???

  def getFileReaderO: UIO[Option[FileRequest.FileReader[FileT]]] =
    fileLengthLargerThanInt.map(b => Option.when(!b)(FileRequest.FileReader(this)))
  def getFileReader: TaskM[FileRequest.FileReader[FileT]] =
    getFileReaderO.someOrElseZIO(ZIO.fail(KError.message.same("File size is too large to create reader, use `forwardToFile` instead")))

  private def contentsAsArray: TaskM[Array[Byte]] = ???
  private def contentsAsString: TaskM[String] = ???

}
object FileRequest {

  final class FileReader[FileT] private[FileRequest] (fileRequest: FileRequest[FileT]) {
    def contentsAsArray: TaskM[Array[Byte]] = fileRequest.contentsAsArray
    def contentsAsString: TaskM[String] = fileRequest.contentsAsString
  }

}
