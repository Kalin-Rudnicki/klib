package klib

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._

object Testing extends App {

  implicit class IOOps[T](t: => T) {

    def toIo: IO[T] =
      IO(t)

  }

  def makeIo(i: Int, err: Boolean = false): ??[Int] =
    ?? {
      if (err)
        throw Message(s"Error on #$i")

      println(s"[$i]")
      i
    }

  val io1 = makeIo(1)
  val io2 = makeIo(2)
  val io3 = makeIo(3, true)

  val ioSum1 =
    ado[??].join(io1, io1, io2, io3).map {
      case (_1, _1_2, _2, _3) =>
        _1 + _1_2 + _2 + _3
    }

  val ioSum2 =
    ado[??].join(ioSum1, ioSum1, ?? { println("Test"); 3 }).map {
      case (_1, _2, _3) =>
        _1 + _2 + _3
    }

  val ioSum3 =
    ado[??].join(ioSum2, ioSum2).map {
      case (_1, _2) =>
        _1 + _2
    }

  def log(level: Logger.LogLevel)(src: Logger.Source)(t: Throwable): Unit = {
    src.log(level, t.getMessage)
    src.indented() { src =>
      t.getStackTrace.foreach(src.debug(_))
    }
  }

  execErrorAccumulator(Logger(Logger.LogLevel.Detailed))(ioSum3.run)(log(Logger.LogLevel.Error))(
    log(Logger.LogLevel.Warning),
  ) { src => res =>
    src.print(res)
  }

}
