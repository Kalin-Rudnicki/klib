package klib.utils

private[this] def commaify(str: String): String = {
  def placeCommas(chars: List[Char]): String =
    chars.reverse.grouped(3).toList.reverseMap(_.reverse.mkString).mkString(",")

  str.toList match {
    case '-' :: chars => "-" + placeCommas(chars)
    case chars        => placeCommas(chars)
  }
}

extension (self: Long) {

  def toStringCommas: String =
    commaify(self.toString)

}

extension (self: Double) {

  def toStringCommas(showEmptyDecimal: Boolean): String = {
    def convert(int: String, fract: String): String =
      (commaify(int) :: Option.when(showEmptyDecimal || fract != "0")(fract).toList).mkString(".")

    BigDecimal(self).toString.split("\\.") match {
      case Array(int, fract) => convert(int, fract)
      case Array(int)        => convert(int, "0")
      case _                 => ???
    }
  }

}
