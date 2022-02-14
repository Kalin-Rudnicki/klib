package klib.web

import zio.*

import klib.utils.*

// TODO (KR) : Need some sort of typeClass for this, so that it can be handled differently on different platforms
final class FileRequest[FileT] {

  def forwardToFile(file: FileT): IO[Message, Unit] = ???

  def fileLengthLargerThanInt: UIO[Boolean] = ???

  def getFileReaderO: UIO[Option[FileRequest.FileReader[FileT]]] =
    fileLengthLargerThanInt.map(b => Option.when(!b)(FileRequest.FileReader(this)))
  def getFileReader: IO[Message, FileRequest.FileReader[FileT]] =
    getFileReaderO.someOrElseZIO(ZIO.fail(Message.same("File size is too large to create reader, use `forwardToFile` instead")))

  private def contentsAsArray: IO[Message, Array[Byte]] = ???
  private def contentsAsString: IO[Message, String] = ???

}
object FileRequest {

  final class FileReader[FileT] private[FileRequest] (fileRequest: FileRequest[FileT]) {
    def contentsAsArray: IO[Message, Array[Byte]] = fileRequest.contentsAsArray
    def contentsAsString: IO[Message, String] = fileRequest.contentsAsString
  }

}
