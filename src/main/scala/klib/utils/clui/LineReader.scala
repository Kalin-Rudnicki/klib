package klib.utils.clui

import klib.fp.types._

trait LineReader {

  def readLine(isStart: Boolean): IO[String]

}

object LineReader {

  def stdIn(
      startPrompt: String,
      continuePrompt: String,
  ): LineReader =
    isStart => IO(scala.io.StdIn.readLine(if (isStart) startPrompt else continuePrompt))

  // TODO (KR) : def fromList(list: List[String])

}
