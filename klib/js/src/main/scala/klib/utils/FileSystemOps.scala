package klib.utils

import zio.*

extension (fs: FileSystem.type) {

  def jsUnimplemented: TaskLayerM[FileSystem] =
    ZLayer.succeed {
      new FileSystem {
        override def createFileObject(path: String): TaskM[File] = ZIO.fail(KError.message.???)
        override def roots: TaskM[Array[File]] = ZIO.fail(KError.message.???)
      }
    }

}
