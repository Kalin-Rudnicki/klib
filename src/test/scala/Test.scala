//

import klib.fp.types._
import klib.Implicits._
import klib.utils.{Color, ColorString, Timer}

object Test extends App {

  println("(123,_456,_789)".replaceColor("\\d+", Color.Named.Red))

}
