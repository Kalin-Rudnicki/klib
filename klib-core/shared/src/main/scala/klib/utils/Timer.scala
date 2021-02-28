package klib.utils

final class Timer {

  val createdAt: Long = System.currentTimeMillis

  def time[R](r: => R): (Long, Long, R) = {
    val start = System.currentTimeMillis
    val res = r
    val end = System.currentTimeMillis
    (end - start, end - createdAt, res)
  }

  def elapsed: Long = System.currentTimeMillis - createdAt

}

object Timer {

  def time[R](r: => R): (Long, R) = {
    val start = System.currentTimeMillis
    val res = r
    val end = System.currentTimeMillis
    (end - start, res)
  }

  def formatMs(ms: Long): String =
    s"${ms}ms"

  def formatSecs(ms: Long): String =
    s"${ms.toDouble / 1000}s"

  def formatFlex(ms: Long): String =
    if (ms <= 0)
      "0 ms"
    else {
      def split(l: Long, d: Int): (Long, Long) =
        (l / d, l % d)

      def build(items: (Long, String, String)*): String = {
        def loop(
            queue: List[(Long, String, String)],
            stack: List[String],
        ): String =
          queue match {
            case (t, l, p) :: tail =>
              if (t == 0)
                loop(tail, stack)
              else if (t == 1)
                loop(tail, s"$t $l" :: stack)
              else
                loop(tail, s"$t $l$p" :: stack)
            case Nil =>
              stack.reverse.mkString(", ")
          }

        loop(items.toList, Nil)
      }

      val (secs, rMs) = split(ms, 1000)
      val (mins, rSecs) = split(secs, 60)
      val (hrs, rMins) = split(mins, 60)
      val rHrs = hrs

      build(
        (rHrs, "hr", "s"),
        (rMins, "min", "s"),
        (rSecs, "sec", "s"),
        (rMs, "ms", ""),
      )
    }

}
