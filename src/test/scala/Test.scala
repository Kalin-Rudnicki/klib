//

import klib.fp.types._
import klib.fp.utils._
import klib.fp.Implicits._

object Test extends App {

  val (result, warnings, errors) =
    ado[?]
      .join(
        0.alive.withWarnings(Message("Warning 1")),
        0.alive.withWarnings(Message("Warning 2"), Message("Warning 3")),
      )
      .flatMap {
        case (_1, _2) =>
          val sum = _1 + _2

          def tryDiv(a: Int, b: Int): ?[Int] =
            if (b != 0)
              (a / b).alive
            else
              Dead(List(Message(s"$a / 0 : Div/0 error")))

          ado[?].join(
            sum.alive,
            tryDiv(_1, _2),
            tryDiv(sum, _1),
            tryDiv(_2, sum),
          )
      }
      .toTuple

  println(s"Result: $result")
  println(s"Warnings: ${warnings.map(_.getMessage).mkString(", ")}")
  println(s"Errors: ${errors.map(_.getMessage).mkString(", ")}")

}
