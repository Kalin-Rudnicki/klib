package klib.webServer

import java.io.{File => JavaFile}

import scalatags.Text.Frag

import klib.fp.types._

sealed trait MatchResult
object MatchResult {
  object FailedMatch extends MatchResult
  final case class Html(html: Frag) extends MatchResult
  final case class Raw(contentType: String, content: String) extends MatchResult
  final case class File(file: JavaFile, contentType: Maybe[String] = None) extends MatchResult
}
