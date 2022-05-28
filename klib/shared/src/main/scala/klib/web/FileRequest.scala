package klib.web

import zio.*

import klib.utils.*

// TODO (KR) : Need some sort of typeClass for this, so that it can be handled differently on different platforms
final class FileRequest[FileT] {

  // TODO (KR) :
  def forwardToFile(file: FileT): KTask[Unit] = ???

  // TODO (KR) :
  def fileLengthLargerThanInt: UIO[Boolean] = ???

  def getFileReaderOpt: UIO[Option[FileRequest.FileReader[FileT]]] =
    fileLengthLargerThanInt.map(b => Option.when(!b)(FileRequest.FileReader(this)))
  def getFileReader: KTask[FileRequest.FileReader[FileT]] =
    getFileReaderOpt.someOrFailKError(KError.Unexpected("File size is too large to create reader, use `forwardToFile` instead"))

  // TODO (KR) :
  private def contentsAsArray: KTask[Array[Byte]] = ???
  private def contentsAsString: KTask[String] = ???

}
object FileRequest {

  final class FileReader[FileT] private[FileRequest] (fileRequest: FileRequest[FileT]) {
    def contentsAsArray: KTask[Array[Byte]] = fileRequest.contentsAsArray
    def contentsAsString: KTask[String] = fileRequest.contentsAsString
  }

}
