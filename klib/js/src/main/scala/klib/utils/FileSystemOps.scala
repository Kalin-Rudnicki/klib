package klib.utils

import zio.*

extension (fs: FileSystem.type) {

  def jsUnimplemented: KTaskLayer[FileSystem] =
    ZLayer.succeed {
      new FileSystem {
        override def createFileObject(path: String): KTask[File] = ZIO.failNEL(KError.???("FileSystem.createFileObject"))
        override def roots: KTask[Array[File]] = ZIO.failNEL(KError.???("FileSystem.roots"))
      }
    }

}
