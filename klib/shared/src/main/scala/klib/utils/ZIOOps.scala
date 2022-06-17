package klib.utils

import cats.data.*
import cats.instances.AllInstances
import cats.syntax.either.*
import cats.syntax.list.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import scala.annotation.tailrec
import scala.annotation.targetName
import zio.*
import zio.json.*
import zio.stream.*

// format: off
// =====| ZIO |=====
// TODO (KR) : Add KZIO?
//           : - Which makes more sense?
//           :   - type KZIO[-R, +E, +A] = ZIO[R, NonEmptyList[Either[E, KError]], A]
//           :   - type KZIO[-R, +E, +A] = ZIO[R, Either[E, NonEmptyList[KError]], A]
//           :   - (I think the first one)
//           : - Add for ZLayer?
//           : - Add for ZStream?

type KTask[    +A] =  ZIO[Any, NonEmptyList[KError], A]
type  KRIO[-R, +A] =  ZIO[R,   NonEmptyList[KError], A]

type SKTask[    +A] = KRIO[Executable.Env,     A]
type  SKRIO[-R, +A] = KRIO[Executable.Env & R, A]

// =====| ZLayer |=====
type KTaskLayer[    +A] = ZLayer[Any, NonEmptyList[KError], A]
type    KRLayer[-R, +A] = ZLayer[R,   NonEmptyList[KError], A]

// =====| ZStream |=====
type KTaskStream[    +A] = ZStream[Any, NonEmptyList[KError], A]
type    KRStream[-R, +A] = ZStream[R,   NonEmptyList[KError], A]
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

  def failIf[E](pred: => Boolean, e: => E): IO[E, Unit] =
    ZIO.cond(pred, (), e)
  def failIfNEL[E](pred: => Boolean, e0: => E, eN: => E*): IO[NonEmptyList[E], Unit] =
    ZIO.cond(pred, (), NonEmptyList(e0, eN.toList))

  def failUnless[E](pred: => Boolean, e: => E): IO[E, Unit] =
    ZIO.cond(!pred, (), e)
  def failUnlessNEL[E](pred: => Boolean, e0: => E, eN: => E*): IO[NonEmptyList[E], Unit] =
    ZIO.cond(!pred, (), NonEmptyList(e0, eN.toList))

  def kAttempt[A](mapError: Throwable => KError)(thunk: => A): KTask[A] =
    ZIO.attempt(thunk).mapErrorToNEL(mapError)

  def kAttempt[A](message: => String, errorType: => KError.ErrorType)(thunk: => A)(implicit trace: Trace): KTask[A] =
    ZIO.kAttempt { throwable =>
      errorType match {
        case KError.ErrorType.Unexpected                   => KError.Unexpected(message, throwable)(trace)
        case KError.ErrorType.SystemFailure                => KError.SystemFailure(message, throwable)(trace)
        case KError.ErrorType.User                         => KError.UserError(message, throwable)(trace)
        case KError.ErrorType.ExternalService(serviceName) => KError.ExternalService(serviceName)(message, throwable)(trace)
      }
    }(thunk)

  def kAttempt[A](message: => String)(thunk: => A)(implicit trace: Trace): KTask[A] =
    kAttempt(message, KError.ErrorType.Unexpected)(thunk)(trace)

  def failNEL[E](e0: E, eN: E*): IO[NonEmptyList[E], Nothing] =
    ZIO.fail(NonEmptyList(e0, eN.toList))

  def fromOptionKError[A](option: => Option[A])(e: => KError): KTask[A] =
    ZIO.fromOption(option).orElseFail(NonEmptyList.one(e))

  def fromEitherKError[E, A](either: => Either[E, A])(mapE: E => KError): KTask[A] =
    ZIO.fromEither(either).mapErrorToNEL(mapE)

  def acquireReleaseClosable[R, E, A <: AutoCloseable](acquire: => ZIO[R, E, A]): ZIO[R & Scope, E, A] =
    ZIO.acquireRelease(acquire)(c => ZIO.kAttempt(s"Unable to auto-close : $c")(c.close()).orDieKError)

}

// =====| KError Mapping |=====

extension [R, E, A](zio: ZIO[R, E, A]) {

  def mapErrorToNEL[E2](f: E => E2): ZIO[R, NonEmptyList[E2], A] =
    zio.mapError(e => NonEmptyList.one(f(e)))

  def toErrorNEL: ZIO[R, NonEmptyList[E], A] =
    zio.mapError(NonEmptyList.one)

}
extension [R, E, A](zLayer: ZLayer[R, E, A]) {

  def mapErrorToNEL[E2](f: E => E2): ZLayer[R, NonEmptyList[E2], A] =
    zLayer.mapError(e => NonEmptyList.one(f(e)))

  def toErrorNEL: ZLayer[R, NonEmptyList[E], A] =
    zLayer.mapError(NonEmptyList.one)

}
extension [R, E, A](zStream: ZStream[R, E, A]) {

  def mapErrorToNEL[E2](f: E => E2): ZStream[R, NonEmptyList[E2], A] =
    zStream.mapError(e => NonEmptyList.one(f(e)))

  def toErrorNEL: ZStream[R, NonEmptyList[E], A] =
    zStream.mapError(NonEmptyList.one)

}

extension [R, E, A](zio: ZIO[R, NonEmptyList[E], A]) {

  def mapErrorNEL[E2](f: E => E2): ZIO[R, NonEmptyList[E2], A] =
    zio.mapError(_.map(f))

}
extension [R, E, A](zLayer: ZLayer[R, NonEmptyList[E], A]) {

  def mapErrorNEL[E2](f: E => E2): ZLayer[R, NonEmptyList[E2], A] =
    zLayer.mapError(_.map(f))

}
extension [R, E, A](zStream: ZStream[R, NonEmptyList[E], A]) {

  def mapErrorNEL[E2](f: E => E2): ZStream[R, NonEmptyList[E2], A] =
    zStream.mapError(_.map(f))

}

extension [R, A](kRIO: KRIO[R, A]) {

  def orDieKError: URIO[R, A] =
    kRIO.mapError(KError.WrappedKErrors(_)).orDie

  def catchCauseKError: KRIO[R, A] =
    kRIO.foldCauseZIO(
      {
        case fail: Cause.Fail[NonEmptyList[KError]] => ZIO.failCause(fail)
        case Cause.Die(throwable, trace) =>
          ZIO.fail(
            KError.unwrapOr(
              throwable,
              KError.Unexpected("Experienced unexpected ZIO die", _, trace.stackTrace),
            ),
          )
        case cause =>
          ZIO.failNEL(KError.Unexpected(s"Experienced unexpected ZIO cause:\n$cause"))
      },
      ZIO.succeed,
    )

  def someOrFailKError[A2](e0: => KError, eN: => KError*)(implicit ev: A <:< Option[A2]): KRIO[R, A2] =
    kRIO.someOrFail(NonEmptyList(e0, eN.toList))

}
extension [R, A](kRLayer: KRLayer[R, A]) {
  def orDieKError: URLayer[R, A] =
    kRLayer.mapError(KError.WrappedKErrors(_)).orDie
}
// TODO (KR) : Implement for RStreamM as well?

extension [R, A](kRIO: KRIO[R, A]) {

  def dumpErrorsAndContinue: URIO[R & RunMode & Logger, Option[A]] =
    kRIO.dumpErrorsAndContinue(Logger.LogLevel.Error)

  def dumpErrorsAndContinue(errorLevel: Logger.LogLevel): URIO[R & RunMode & Logger, Option[A]] =
    kRIO.foldZIO(
      errors =>
        ZIO
          .service[RunMode]
          .flatMap { runMode => Logger.execute.all(errors.toList.map(_.formatEvent(runMode, errorLevel))) }
          .as(None),
      ZIO.some,
    )

  // TODO (KR) : Error recovery/retry

  def <**>[B](other: KRIO[R, B])(implicit zippable: Zippable[A, B]): KRIO[R, zippable.Out] =
    (kRIO.either <*> other.either).flatMap { pair =>
      ZIO.fromEither { pair.parMapN(zippable.zip) }
    }

  def <**[B](other: KRIO[R, B]): KRIO[R, A] =
    (kRIO.either <*> other.either).flatMap { pair =>
      ZIO.fromEither { pair.parMapN((a, _) => a) }
    }

  def **>[B](other: KRIO[R, B]): KRIO[R, B] =
    (kRIO.either <*> other.either).flatMap { pair =>
      ZIO.fromEither { pair.parMapN((_, b) => b) }
    }

  def <&&>[B](other: KRIO[R, B])(implicit zippable: Zippable[A, B]): KRIO[R, zippable.Out] =
    (kRIO.either <&> other.either).flatMap { pair =>
      ZIO.fromEither { pair.parMapN(zippable.zip) }
    }

  def <&&[B](other: KRIO[R, B]): KRIO[R, A] =
    (kRIO.either <&> other.either).flatMap { pair =>
      ZIO.fromEither { pair.parMapN((a, _) => a) }
    }

  def &&>[B](other: KRIO[R, B]): KRIO[R, B] =
    (kRIO.either <&> other.either).flatMap { pair =>
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
