package klib.utils

import scala.annotation.tailrec

import cats.data.*
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import zio.*

type TaskM[A] = ZIO[Any, Message, A]
type TaskMNel[A] = ZIO[Any, NonEmptyList[Message], A]
type RIOM[R, A] = ZIO[R, Message, A]
type RIOMNel[R, A] = ZIO[R, NonEmptyList[Message], A]

extension (zio: ZIO.type) {

  def traverse[R, E, A, B](nel: NonEmptyList[A])(f: A => ZIO[R, E, B]): ZIO[R, NonEmptyList[E], NonEmptyList[B]] =
    ZIO
      .foreach(NonEmptyChunk(nel.head, nel.tail*))(f(_).either)
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
  def traverseNEL[R, E, A, B](nel: NonEmptyList[A])(f: A => ZIO[R, NonEmptyList[E], B]): ZIO[R, NonEmptyList[E], NonEmptyList[B]] =
    traverse(nel)(f).mapError(_.flatMap(identity))

  def traverse[R, E, A, B](list: List[A])(f: A => ZIO[R, E, B]): ZIO[R, NonEmptyList[E], List[B]] =
    list.toNel match {
      case Some(nel) => traverse(nel)(f).map(_.toList)
      case None      => ZIO.succeed(Nil)
    }
  def traverseNEL[R, E, A, B](list: List[A])(f: A => ZIO[R, NonEmptyList[E], B]): ZIO[R, NonEmptyList[E], List[B]] =
    traverse(list)(f).mapError(_.flatMap(identity))

}

extension [R, A](zio: ZIO[R, Throwable, A]) {

  def messageError: ZIO[R, Message, A] =
    zio.mapError(Message.fromThrowable(_))

}
extension [R, E, A](zio: ZIO[R, E, A]) {

  def nelError: ZIO[R, NonEmptyList[E], A] =
    zio.mapError(NonEmptyList.one)

}

extension [R, A](zlayer: ZLayer[R, Throwable, A]) {

  def messageError: ZLayer[R, Message, A] =
    zlayer.mapError(Message.fromThrowable(_))

}

extension [R, E, A](zlayer: ZLayer[R, E, A]) {

  def nelError: ZLayer[R, NonEmptyList[E], A] =
    zlayer.mapError(NonEmptyList.one)

}
