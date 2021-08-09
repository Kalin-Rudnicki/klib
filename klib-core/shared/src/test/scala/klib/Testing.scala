package klib

import java.io.File

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  val file = new File("/home/kalin/Desktop/PhotosToUpload/Camera/20161223_154323.jpg")

  ImageUtils
    .getMetaData(file)
    .runSyncOrExit(None)

}
