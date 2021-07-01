package klib.fp.types

import scala.annotation.tailrec

import klib.fp.typeclass._

final case class NonEmptyList[+A](
    head: A,
    tail: List[A],
) {

  def size: Int =
    tail.size + 1

  @inline
  def toList: List[A] =
    head :: tail

  @inline
  def ::[A2 >: A](a: A2): NonEmptyList[A2] =
    NonEmptyList(a, head :: tail)

  @inline
  def :::[A2 >: A](as: List[A2]): NonEmptyList[A2] =
    as match {
      case h :: t =>
        NonEmptyList(h, t ::: head :: tail)
      case Nil =>
        this
    }

  @inline
  def :::[A2 >: A](as: NonEmptyList[A2]): NonEmptyList[A2] =
    NonEmptyList(as.head, as.tail ::: head :: tail)

  def reverse: NonEmptyList[A] = {
    @tailrec
    def loop(
        queue: List[A],
        stack: NonEmptyList[A],
    ): NonEmptyList[A] =
      queue match {
        case head :: tail =>
          loop(
            tail,
            head :: stack,
          )
        case Nil =>
          stack
      }

    loop(
      tail,
      NonEmptyList(head, Nil),
    )
  }

  def zipWithIndex: NonEmptyList[(A, Int)] = {
    @tailrec
    def loop(
        counter: Int,
        queue: List[A],
        stack: List[(A, Int)],
    ): List[(A, Int)] =
      queue match {
        case head :: tail =>
          loop(
            counter + 1,
            tail,
            (head, counter) :: stack,
          )
        case Nil =>
          stack.reverse
      }

    NonEmptyList(
      (head, 0),
      loop(1, tail, Nil),
    )
  }

  def zip[B](other: NonEmptyList[B]): NonEmptyList[(A, B)] = {
    @tailrec
    def loop(
        a: List[A],
        b: List[B],
        ab: NonEmptyList[(A, B)],
    ): NonEmptyList[(A, B)] =
      a match {
        case aHead :: aTail =>
          b match {
            case bHead :: bTail =>
              loop(
                aTail,
                bTail,
                (aHead, bHead) :: ab,
              )
            case Nil =>
              ab.reverse
          }
        case Nil =>
          ab.reverse
      }

    loop(
      this.tail,
      other.tail,
      NonEmptyList((this.head, other.head), Nil),
    )
  }

  def zipFully[A2 >: A, B](other: NonEmptyList[B], fillA: A2, fillB: B): NonEmptyList[(A2, B)] = {
    @tailrec
    def loop(
        a: List[A],
        b: List[B],
        ab: NonEmptyList[(A2, B)],
    ): NonEmptyList[(A2, B)] =
      a match {
        case aHead :: aTail =>
          b match {
            case bHead :: bTail =>
              loop(
                aTail,
                bTail,
                (aHead, bHead) :: ab,
              )
            case Nil =>
              loop(
                aTail,
                Nil,
                (aHead, fillB) :: ab,
              )
          }
        case Nil =>
          b match {
            case bHead :: bTail =>
              loop(
                Nil,
                bTail,
                (fillA, bHead) :: ab,
              )
            case Nil =>
              ab.reverse
          }
      }

    loop(
      this.tail,
      other.tail,
      NonEmptyList((this.head, other.head), Nil),
    )
  }

  def zipExactly[B](other: NonEmptyList[B]): Maybe[NonEmptyList[(A, B)]] = {
    @tailrec
    def loop(
        a: List[A],
        b: List[B],
        ab: NonEmptyList[(A, B)],
    ): Maybe[NonEmptyList[(A, B)]] =
      a match {
        case aHead :: aTail =>
          b match {
            case bHead :: bTail =>
              loop(
                aTail,
                bTail,
                (aHead, bHead) :: ab,
              )
            case Nil =>
              None
          }
        case Nil =>
          b match {
            case Nil =>
              Some(ab.reverse)
            case _ =>
              None
          }
      }

    loop(
      this.tail,
      other.tail,
      NonEmptyList((this.head, other.head), Nil),
    )
  }

  override def toString: String =
    s"NonEmptyList(${(head :: tail).mkString(", ")})"

}

object NonEmptyList {

  def nel[A](head: A, tails: A*): NonEmptyList[A] =
    NonEmptyList(head, tails.toList)

  object extensions {

    extension [A](list: List[A]) {

      def toNel: Maybe[NonEmptyList[A]] =
        list match {
          case head :: tail =>
            Some(NonEmptyList(head, tail))
          case Nil =>
            None
        }

    }

  }

  object instances {

    given nonEmptyListMonad: Monad[NonEmptyList] with {

      extension [A](t: NonEmptyList[A]) {

        def map[B](f: A => B): NonEmptyList[B] =
          NonEmptyList(f(t.head), t.tail.map(f))

        def apply[B](f: NonEmptyList[A => B]): NonEmptyList[B] =
          f.map { f => t.map(f) }.flatten

      }

      def pure[I](i: => I): NonEmptyList[I] =
        NonEmptyList(i, Nil)

      extension [A](t: NonEmptyList[NonEmptyList[A]])
        def flatten: NonEmptyList[A] =
          NonEmptyList(
            t.head.head,
            t.head.tail ::: t.tail.flatMap(_.toList),
          )

    }

    given nonEmptyListForeach: Foreach[NonEmptyList] with {

      extension [A](t: NonEmptyList[A])
        def foreach(f: A => Unit): Unit = {
          f(t.head)
          t.tail.foreach(f)
        }

    }

  }

}
