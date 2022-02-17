package klib.utils

import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs

import zio.*

extension (fs: FileSystem.type) {

  def currentPlatformJIMFS: ZLayer[Any, Message, FileSystem] =
    FileSystem.layer(Jimfs.newFileSystem(Configuration.forCurrentPlatform()))

  def unixJIMFS: ZLayer[Any, Message, FileSystem] =
    FileSystem.layer(Jimfs.newFileSystem(Configuration.unix()))

}
