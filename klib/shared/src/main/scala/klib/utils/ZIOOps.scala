package klib.utils

import cats.data.*
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import scala.annotation.tailrec
import zio.*
import zio.stream.*

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

  def attemptM[A](effect: => A): TaskM[A] =
    ZIO.attempt(effect).messageError

  def failIf[E](pred: Boolean, e: => E): IO[E, Unit] =
    ZIO.cond(pred, (), e)

  def failUnless[E](pred: Boolean, e: => E): IO[E, Unit] =
    ZIO.cond(!pred, (), e)

}

// =====| Error Mapping |=====

extension [R, A](zio: ZIO[R, Throwable, A]) {

  def messageError: ZIO[R, Message, A] =
    zio.mapError(Message.fromThrowable(_))

}
extension [R, E, A](zio: ZIO[R, E, A]) {

  def nelError: ZIO[R, NonEmptyList[E], A] =
    zio.mapError(NonEmptyList.one)

}

extension [R, A](zLayer: ZLayer[R, Throwable, A]) {

  def messageError: ZLayer[R, Message, A] =
    zLayer.mapError(Message.fromThrowable(_))

}

extension [R, E, A](zLayer: ZLayer[R, E, A]) {

  def nelError: ZLayer[R, NonEmptyList[E], A] =
    zLayer.mapError(NonEmptyList.one)

}

extension [R, A](zStream: ZStream[R, Throwable, A]) {

  def messageError: ZStream[R, Message, A] =
    zStream.mapError(Message.fromThrowable(_))

}

extension [R, E, A](zStream: ZStream[R, E, A]) {

  def nelError: ZStream[R, NonEmptyList[E], A] =
    zStream.mapError(NonEmptyList.one)

}

// =====| Misc |=====

extension [A](self: Iterator[A]) {

  def nextOptionZIO: UIO[Option[A]] =
    ZIO.succeed(self.nextOption)

}

extension [R, E, A](zStream: ZStream[R, E, A]) {

  def toZStreamIterator: UIO[ZStreamIterator[R, E, A]] =
    ZStreamIterator.fromZStream(zStream)

}

extension [A](self: Ref[A]) {

  def modifyEither[E, B](f: A => Either[E, (B, A)]): IO[E, B] =
    self
      .modify { a =>
        val res = f(a)
        (res.map(_._1), res.fold(_ => a, _._2))
      }
      .flatMap(ZIO.fromEither)

}
