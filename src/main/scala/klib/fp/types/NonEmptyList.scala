package klib.fp.types

import klib.fp.typeclass.{ForEach, Monad}

final case class NonEmptyList[+A](
    head: A,
    tail: List[A],
) {

  def toList: List[A] =
    head :: tail

  def ::[A2 >: A](a: A2): NonEmptyList[A2] =
    NonEmptyList(a, head :: tail)

}

object NonEmptyList {

  def nel[A](head: A, tails: A*): NonEmptyList[A] =
    NonEmptyList(head, tails.toList)

  trait Implicits {

    implicit class NonEmptyListListOps[A](list: List[A]) {

      def toNel: Maybe[NonEmptyList[A]] =
        list match {
          case head :: tail =>
            Some(NonEmptyList(head, tail))
          case Nil =>
            None
        }

    }

  }
  object Implicits extends Implicits

  // Instances

  implicit val nonEmptyListMonad: Monad[NonEmptyList] =
    new Monad[NonEmptyList] {

      override def map[A, B](t: NonEmptyList[A], f: A => B): NonEmptyList[B] =
        NonEmptyList(f(t.head), t.tail.map(f))

      override def apply[A, B](t: NonEmptyList[A], f: NonEmptyList[A => B]): NonEmptyList[B] =
        flatten(
          map[A => B, NonEmptyList[B]](
            f,
            f =>
              map[A, B](
                t,
                t => f(t),
              ),
          ),
        )

      override def pure[A](a: A): NonEmptyList[A] =
        NonEmptyList(a, Nil)

      override def flatten[A](t: NonEmptyList[NonEmptyList[A]]): NonEmptyList[A] =
        NonEmptyList(
          t.head.head,
          t.head.tail ::: t.tail.flatMap(_.toList),
        )

    }

  implicit val nonEmptyListForEach: ForEach[NonEmptyList] =
    new ForEach[NonEmptyList] {
      override def forEach[A](t: NonEmptyList[A], f: A => Unit): Unit = {
        f(t.head)
        t.tail.foreach(f)
      }
    }

}
