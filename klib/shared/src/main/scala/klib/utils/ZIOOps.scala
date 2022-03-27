package klib.utils

import cats.data.*
import cats.instances.AllInstances
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import io.circe.Encoder
import scala.annotation.tailrec
import scala.annotation.targetName
import zio.*
import zio.stream.*

// format: off
// =====| ZIOM |=====
type ZIOM[-R, +E, +A] = ZIO[R, KError[E], A]

type  TaskM[        +A] = ZIOM[Any, Nothing, A]
type    IOM[    +E, +A] = ZIOM[Any, E,       A]
type   RIOM[-R,     +A] = ZIOM[R,   Nothing, A]

type  SZIOM[-R, +E, +A] = ZIOM[Executable.BaseEnv & R, E,       A]
type STaskM[        +A] = ZIOM[Executable.BaseEnv,     Nothing, A]
type   SIOM[    +E, +A] = ZIOM[Executable.BaseEnv,     E,       A]
type  SRIOM[-R,     +A] = ZIOM[Executable.BaseEnv & R, Nothing, A]

// =====| ZManagedM |=====
type ZManagedM[-R, +E, +A] = ZManaged[R, KError[E], A]

type TaskManagedM[        +A] = ZManagedM[Any, Nothing, A]
type     ManagedM[    +E, +A] = ZManagedM[Any, E,       A]
type    RManagedM[-R,     +A] = ZManagedM[R,   Nothing, A]

// =====| ZLayerM |=====
type ZLayerM[-R, +E, +A] = ZLayer[R, KError[E], A]

type TaskLayerM[        +A] = ZLayerM[Any, Nothing, A]
type   IOLayerM[    +E, +A] = ZLayerM[Any, E,       A]
type    RLayerM[-R,     +A] = ZLayerM[R,   Nothing, A]

// =====| ZStreamM |=====
type ZStreamM[-R, +E, +A] = ZStream[R, KError[E], A]

type TaskStreamM[        +A] = ZStreamM[Any, Nothing, A]
type   IOStreamM[    +E, +A] = ZStreamM[Any, E,       A]
type    RStreamM[-R,     +A] = ZStreamM[R,   Nothing, A]
// format: on

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

  def failIf[E](pred: Boolean, e: => E): IO[E, Unit] =
    ZIO.cond(pred, (), e)

  def failUnless[E](pred: Boolean, e: => E): IO[E, Unit] =
    ZIO.cond(!pred, (), e)

}

object ZIOM {

  def mapAttempt[E, A](map: (KError.type, KError[Nothing]) => KError[E])(thunk: => A): IOM[E, A] =
    ZIO.attempt(thunk).mapError { throwable => map(KError, KError.throwable(throwable)) }

  def attempt[A](thunk: => A): TaskM[A] =
    ZIO.attempt(thunk).mapError(KError.throwable(_))

  def traverse[R, E, A, B](nel: NonEmptyList[A])(f: A => ZIOM[R, E, B]): ZIOM[R, E, NonEmptyList[B]] =
    ZIO.traverse(nel)(f).mapError(KError.flatten)

  def traverse[R, E, A, B](list: List[A])(f: A => ZIOM[R, E, B]): ZIOM[R, E, List[B]] =
    ZIO.traverse(list)(f).mapError(KError.flatten)

}

// =====| KError Mapping |=====

extension [R, A](zio: ZIO[R, Throwable, A]) {
  def toKlibError: RIOM[R, A] =
    zio.mapError(KError.throwable)
}
extension [R, A](zLayer: ZLayer[R, Throwable, A]) {
  def toKlibError: RLayerM[R, A] =
    zLayer.mapError(KError.throwable)
}
extension [R, A](zStream: ZStream[R, Throwable, A]) {
  def toKlibError: RStreamM[R, A] =
    zStream.mapError(KError.throwable)
}

extension [R, E, A](zioM: ZIOM[R, E, A]) {
  def orDieKlib: URIO[R, A] =
    zioM.mapError(ErrorThrowable.fromError(_)).orDie
}
extension [R, E, A](zLayerM: ZLayerM[R, E, A]) {
  def orDieKlib: URLayer[R, A] =
    zLayerM.mapError(ErrorThrowable.fromError(_)).orDie
}

extension [R, E, A](zioM: ZIOM[R, E, A]) {

  def dumpErrorsAndContinue(errorLevel: Logger.LogLevel, convertE: E => String): URIO[R & RunMode & Logger, Option[A]] =
    zioM.foldZIO(
      _.toLoggerEvent(errorLevel, convertE).flatMap(Logger.execute(_)).as(None),
      ZIO.some,
    )

  def dumpErrorsAndContinue(errorLevel: Logger.LogLevel): URIO[R & RunMode & Logger, Option[A]] =
    dumpErrorsAndContinue(errorLevel, _.toString)

  def dumpJsonErrorsAndContinue(errorLevel: Logger.LogLevel)(implicit encoder: Encoder[E]): URIO[R & RunMode & Logger, Option[A]] =
    dumpErrorsAndContinue(errorLevel, encoder(_).spaces4)

  def recoverFromErrors[B](f: E => RIOM[R, B]): RIOM[R, Either[List[B], A]] = {
    def convertSingleError(error: SingleError[E]): URIO[R, Either[NonEmptyList[SingleError[Nothing]], List[B]]] =
      error match {
        case SingleError.Err(err, _, _) => ???
        case SingleError.Message(devMessage, prodMessage, stackTrace, cause) =>
          cause match {
            case None =>
              ZIO.left(NonEmptyList.one(SingleError.Message(devMessage, prodMessage, stackTrace, None)))
            case Some(cause) =>
              ZIOM
                .traverse(cause.toNEL)(convertSingleError)
                .fold(
                  e => e.toNEL.asLeft,
                  _.parTraverse(identity).map(_.toList.flatten) match {
                    case Right(_) =>
                      NonEmptyList.one(SingleError.Message(devMessage, prodMessage, stackTrace, None)).asLeft
                    case Left(errors) =>
                      (SingleError.Message(devMessage, prodMessage, stackTrace, None) :: errors).asLeft
                  },
                )
          }
        case e: SingleError.Throwable => ZIO.left(NonEmptyList.one(e))
      }

    zioM.foldM(
      error =>
        ZIOM.traverse(error.toNEL)(convertSingleError).flatMap {
          _.parTraverse(identity) match {
            case Right(values) =>
              ZIO.succeed(values.toList.flatten.asLeft)
            case Left(errors) =>
              ZIO.fail(KError(errors))
          }
        },
      ZIO.right,
    )
  }

  def <**>[B](other: ZIOM[R, E, B])(implicit zippable: Zippable[A, B]): ZIOM[R, E, zippable.Out] =
    (zioM.either <*> other.either).flatMap { pair =>
      ZIO.fromEither { pair.parMapN(zippable.zip) }
    }

  def <**[B](other: ZIOM[R, E, B]): ZIOM[R, E, A] =
    (zioM.either <*> other.either).flatMap { pair =>
      ZIO.fromEither { pair.parMapN((a, _) => a) }
    }

  def **>[B](other: ZIOM[R, E, B]): ZIOM[R, E, B] =
    (zioM.either <*> other.either).flatMap { pair =>
      ZIO.fromEither { pair.parMapN((_, b) => b) }
    }

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
