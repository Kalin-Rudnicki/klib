package object klib {

  trait Implicits extends fp.Implicits with utils.Implicits with scalaCompat.Implicits {

    implicit class UnescCharOps(char: Char) {

      def unesc: String =
        unesc("'")

      // TODO (KR) : Add more escaped characters here as they become known
      def unesc(_1: String, _2: Option[String] = None): String = {
        val left = _1
        val right = _2.getOrElse(_1)
        val charText = char match {
          case '\n' =>
            "\\n"
          case '\\' =>
            "\\\\"
          case '\t' =>
            "\\t"
          case '"' =>
            "\\\""
          case c =>
            c.toString
        }
        s"$left$charText$right"
      }

    }

    implicit class UnescStringOps(string: String) {

      def unesc: String =
        unesc("\"")

      def unesc(_1: String, _2: Option[String] = None, f: String => String = s => s): String = {
        // `f` serves the purpose of if you want to wrap in quotes, but also have other text inside the quotes
        val left = _1
        val right = _2.getOrElse(_1)
        val strText = f(string.map(_.unesc("")).mkString)
        s"$left$strText$right"
      }

    }

  }
  object Implicits extends Implicits

}
