package klib.utils

import cats.data.NonEmptyList

object NENil {

  def ::[T](head: T): NonEmptyList[T] = NonEmptyList(head, Nil)

}
