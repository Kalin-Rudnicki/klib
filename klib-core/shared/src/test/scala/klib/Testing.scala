package klib

import klib.Implicits._
import klib.fp.types._
import klib.utils._

object Testing extends App {

  val list1: NonEmptyList[?[Int]] =
    NonEmptyList.nel(
      1.pure[?],
      2.pure[?],
      3.pure[?],
    )

  val list2: NonEmptyList[?[Int]] =
    NonEmptyList.nel(
      1.pure[?],
      2.pure[?],
      ?.dead(Message("3 is dead")),
    )

  val list3: NonEmptyList[?[Int]] =
    NonEmptyList.nel(
      ?.dead(Message("1 is dead")),
      2.pure[?],
      ?.dead(Message("3 is dead")),
    )

  // =====|  |=====

  // ...
  println("=====| List |=====")
  println(list1.toList.traverse)
  println(list2.toList.traverse)
  println(list3.toList.traverse)
  println

  // ...
  println("=====| NonEmptyList |=====")
  println(list1.traverse)
  println(list2.traverse)
  println(list3.traverse)
  println

}
