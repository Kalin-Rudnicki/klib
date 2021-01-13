//

import klib.utils.Timer

object Test extends App {

  println(Timer.formatFlex(0))
  println(Timer.formatFlex(5))
  println(Timer.formatFlex(5001))
  println(Timer.formatFlex(5001000))
  println(Timer.formatFlex(500100050))

}
