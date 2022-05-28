package klib.utils

import com.google.common.jimfs.Configuration
import com.google.common.jimfs.Jimfs
import zio.*

extension (fs: FileSystem.type) {

  def currentPlatformJIMFS: KTaskLayer[FileSystem] =
    FileSystem.layer(Jimfs.newFileSystem(Configuration.forCurrentPlatform()))

  def unixJIMFS: KTaskLayer[FileSystem] =
    FileSystem.layer(Jimfs.newFileSystem(Configuration.unix()))

}
