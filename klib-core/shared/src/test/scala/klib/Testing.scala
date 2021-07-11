package klib

import klib.extensions.{given, _}
import klib.fp.types._
import klib.utils._

object Testing extends App {

  sealed trait Type1T
  type Type1 = Int @@ Type1T

  sealed trait Type2T
  type Type2 = Int @@ Type2T

  val int: Int = 5

  val type1_1: Type1 = int.wrap
  val type1_2: Type1 = int.wrap[Type1]

}
