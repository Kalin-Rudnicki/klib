package klib.utils

import scala.annotation.tailrec

import cats.data.*
import cats.syntax.either._
import cats.syntax.list._
import cats.syntax.option._
import zio.*

extension (zio: ZIO.type) {

  def traverse[R, E, A, B](nel: NonEmptyList[A])(f: A => ZIO[R, E, B]): ZIO[R, NonEmptyList[E], NonEmptyList[B]] =
    ZIO
      .foreach(NonEmptyChunk(nel.head, nel.tail: _*))(f(_).either)
      .flatMap { nec =>
        @tailrec
        def loopErrors(
            queue: List[Either[E, B]],
            errors: NonEmptyList[E],
        ): IO[NonEmptyList[E], Nothing] =
          queue match {
            case head :: tail =>
              head match {
                case Right(_)    => loopErrors(tail, errors)
                case Left(error) => loopErrors(tail, error :: errors)
              }
            case Nil =>
              ZIO.fail(errors.reverse)
          }

        @tailrec
        def loopValues(
            queue: List[Either[E, B]],
            values: NonEmptyList[B],
        ): IO[NonEmptyList[E], NonEmptyList[B]] =
          queue match {
            case head :: tail =>
              head match {
                case Right(value) => loopValues(tail, value :: values)
                case Left(error)  => loopErrors(tail, NonEmptyList.one(error))
              }
            case Nil =>
              ZIO.succeed(values.reverse)
          }

        nec.head match {
          case Right(value) => loopValues(nec.tail.toList, NonEmptyList.one(value))
          case Left(error)  => loopErrors(nec.tail.toList, NonEmptyList.one(error))
        }
      }

  def traverse[R, E, A, B](list: List[A])(f: A => ZIO[R, E, B]): ZIO[R, NonEmptyList[E], List[B]] =
    list.toNel match {
      case Some(nel) => traverse(nel)(f).map(_.toList)
      case None      => ZIO.succeed(Nil)
    }

}

extension [R, A](zio: ZIO[R, Throwable, A]) {

  def messageError(userMessage: String): ZIO[R, Message, A] =
    zio.mapError(Message.fromThrowable(_, userMessage.some))

  def messageError: ZIO[R, Message, A] =
    zio.mapError(Message.fromThrowable(_))

}
