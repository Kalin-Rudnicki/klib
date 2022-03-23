package klib.utils

import cats.data.*
import zio.Zippable

extension [L, R](self: EitherNel[L, R]) {

  def accumulate[R2](other: EitherNel[L, R2])(implicit zip: Zippable[R, R2]): EitherNel[L, zip.Out] =
    self match {
      case Right(selfR) =>
        other match {
          case Right(otherR) => Right(zip.zip(selfR, otherR))
          case Left(otherL)  => Left(otherL)
        }
      case Left(selfL) =>
        other match {
          case Right(otherR) => Left(selfL)
          case Left(otherL)  => Left(selfL ::: otherL)
        }
    }

}
