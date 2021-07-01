package klib

import klib.fp.types._

object extensions {
  export fp.extensions._
  export utils.extensions._

  extension (char: Char) {

    def unesc: String =
      char.unesc("'", None)

    def unesc(_1: String, _2: Maybe[String] = None): String = {
      val left = _1
      val right = _2.getOrElse(_1)
      val charText = char match {
        case '\n' =>
          "\\n"
        case '\\' =>
          "\\\\"
        case '\t' =>
          "\\t"
        case c =>
          c.toString
      }
      s"$left$charText$right"
    }

  }

  extension (string: String) {

    def unesc: String =
      string.unesc("\"")

    def unesc(_1: String): String =
      string.unesc(_1, None, identity)

    def unesc(
        _1: String,
        _2: Maybe[String], // None
        f: String => String, // identity
    ): String = {
      // `f` serves the purpose of if you want to wrap in quotes, but also have other text inside the quotes
      val left = _1
      val right = _2.getOrElse(_1)
      val strText = f(string.map(_.unesc("")).mkString)
      s"$left$strText$right"
    }

  }

}
