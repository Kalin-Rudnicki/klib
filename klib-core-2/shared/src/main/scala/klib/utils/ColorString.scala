package klib.utils

import scala.annotation.tailrec

import cats.data.*
import cats.syntax.option.*
import cats.syntax.list.*

sealed trait ColorString {

  def copy(cpF: ColorString.Color => ColorString.Color): ColorString

  def overwrite(color: ColorString.Color): ColorString =
    this.copy(_.overwrite(color))

  def underwrite(color: ColorString.Color): ColorString =
    this.copy(_.underwrite(color))

  // =====| Foreground |=====

  def black: ColorString =
    this.copy(_.copy(fg = Color.Named.Black.some))

  def red: ColorString =
    this.copy(_.copy(fg = Color.Named.Red.some))

  def green: ColorString =
    this.copy(_.copy(fg = Color.Named.Green.some))

  def yellow: ColorString =
    this.copy(_.copy(fg = Color.Named.Yellow.some))

  def blue: ColorString =
    this.copy(_.copy(fg = Color.Named.Blue.some))

  def magenta: ColorString =
    this.copy(_.copy(fg = Color.Named.Magenta.some))

  def cyan: ColorString =
    this.copy(_.copy(fg = Color.Named.Cyan.some))

  def white: ColorString =
    this.copy(_.copy(fg = Color.Named.White.some))

  def rgb(r: Int, g: Int, b: Int): ColorString =
    this.copy(_.copy(fg = Color.RGB(r, g, b).some))

  def dflt: ColorString =
    this.copy(_.copy(fg = Color.Default.some))

  def noFg: ColorString =
    this.copy(_.copy(fg = None))

  // =====| Background |=====

  def blackBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Black.some))

  def redBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Red.some))

  def greenBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Green.some))

  def yellowBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Yellow.some))

  def blueBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Blue.some))

  def magentaBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Magenta.some))

  def cyanBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Cyan.some))

  def whiteBg: ColorString =
    this.copy(_.copy(bg = Color.Named.White.some))

  def rgbBg(r: Int, g: Int, b: Int): ColorString =
    this.copy(_.copy(bg = Color.RGB(r, g, b).some))

  def dfltBg: ColorString =
    this.copy(_.copy(bg = Color.Default.some))

  def noBg: ColorString =
    this.copy(_.copy(bg = None))

  // =====| ... |=====

  def toColorString: ColorString =
    this

  def +(other: ColorString): ColorString =
    this match {
      case ColorString.Simple(color, str) =>
        ColorString.Complex(color, (str.some, other) :: Nil, None)
      case ColorString.Complex(color, pairs, tail) =>
        ColorString.Complex(color, pairs :+ (tail, other), None)
    }

  def +(otherStr: String): ColorString =
    this match {
      case ColorString.Simple(color, str) =>
        ColorString.Simple(color, str + otherStr)
      case ColorString.Complex(color, pairs, tail) =>
        ColorString.Complex(color, pairs, tail.fold(otherStr)(_ + otherStr).some)
    }

  def split(splitStr: String): List[ColorString] =
    this match {
      case ColorString.Simple(color, str) =>
        str.split(splitStr).map(ColorString.Simple(color, _)).toList
      case ColorString.Complex(color, pairs, tail) =>
        List(
          pairs.flatMap { case (oStr, cStr) =>
            List(
              oStr.toList.flatMap {
                _.split(splitStr).map(ColorString.Simple(color, _))
              },
              cStr.split(splitStr),
            ).flatten
          },
          tail.toList.flatMap {
            _.split(splitStr).map(ColorString.Simple(color, _))
          },
        ).flatten
    }

  def length: Int =
    this match {
      case ColorString.Simple(_, str) =>
        str.length
      case ColorString.Complex(_, pairs, tail) =>
        pairs.map { case (oStr, cStr) =>
          oStr.fold(0)(_.length) + cStr.length
        }.sum + tail.fold(0)(_.length)
    }

  def toRawString: String = {
    val stringBuilder: StringBuilder = new StringBuilder

    def rec(
        colorString: ColorString,
    ): Unit =
      colorString match {
        case ColorString.Simple(_, str) =>
          stringBuilder.append(str)
        case ColorString.Complex(_, pairs, tail) =>
          pairs.foreach { case (str, cStr) =>
            str.foreach(stringBuilder.append)
            rec(cStr)
          }
          tail.foreach(stringBuilder.append)
      }

    rec(this)
    stringBuilder.toString
  }

  override def toString: String = {
    val stringBuilder: StringBuilder = new StringBuilder

    def append(
        cs: ColorString.ColorState,
        c: ColorString.Color,
        str: String,
    ): ColorString.ColorState =
      c.diffWithState(cs) match {
        case Some((ansi, ncs)) =>
          stringBuilder.append(ansi).append(str)
          ncs
        case None =>
          stringBuilder.append(str)
          cs
      }

    def rec(
        colorString: ColorString,
        colorState: ColorString.ColorState,
    ): ColorString.ColorState =
      colorString match {
        case ColorString.Simple(color, str) =>
          append(
            colorState,
            color,
            str,
          )
        case ColorString.Complex(color, pairs, tail) =>
          val afterPairs =
            pairs.foldLeft(colorState) { case (ccs, (oStr, cStr)) =>
              val afterOStr =
                oStr.map(append(ccs, color, _)).getOrElse(ccs)
              val afterCStr =
                rec(
                  cStr,
                  afterOStr,
                )

              afterCStr
            }
          val afterTail =
            tail.map(append(afterPairs, color, _)).getOrElse(afterPairs)

          afterTail
      }

    val finalColorState = rec(
      this,
      ColorString.ColorState.Default,
    )
    ColorString.Color.Default.diffWithState(finalColorState).foreach { case (ansi, _) =>
      stringBuilder.append(ansi)
    }
    stringBuilder.toString
  }

}

object ColorString {
  import klib.utils.Color as RawColor

  private def ansiEscape(codes: NonEmptyList[String]): String =
    s"\u001b[${codes.toList.mkString(";")}m"

  final case class Color(
      fg: Option[RawColor],
      bg: Option[RawColor],
  ) {

    def overwrite(other: Color): Color =
      Color(
        fg = other.fg.orElse(fg),
        bg = other.bg.orElse(bg),
      )

    def underwrite(other: Color): Color =
      Color(
        fg = fg.orElse(other.fg),
        bg = bg.orElse(other.bg),
      )

    // Does a diff, to make sure any coloring is needed
    def diffWithState(colorState: ColorState): Option[(String, ColorState)] =
      (fg.filterNot(_ == colorState.fg), bg.filterNot(_ == colorState.bg)) match {
        case (Some(fg), Some(bg)) =>
          (
            ansiEscape(NonEmptyList.of(fg.fgMod, bg.bgMod)),
            colorState.copy(fg = fg, bg = bg),
          ).some
        case (Some(fg), None) =>
          (
            ansiEscape(NonEmptyList.of(fg.fgMod)),
            colorState.copy(fg = fg),
          ).some
        case (None, Some(bg)) =>
          (
            ansiEscape(NonEmptyList.of(bg.bgMod)),
            colorState.copy(bg = bg),
          ).some
        case (None, None) =>
          None
      }

    def toColorState: ColorState =
      ColorState(
        fg.getOrElse(RawColor.Default),
        bg.getOrElse(RawColor.Default),
      )

  }
  object Color {
    val Empty: Color = Color(fg = None, bg = None)
    val Default: Color = Color(fg = RawColor.Default.some, bg = RawColor.Default.some)
  }

  final case class ColorState(
      fg: RawColor,
      bg: RawColor,
  ) {

    def toColor: Color = Color(fg = fg.some, bg = bg.some)

    def colorizeAndDeColorize(surroundings: ColorState): Option[(String, String)] = {
      (this.fg != surroundings.fg, this.bg != surroundings.bg) match {
        case (true, true) =>
          (
            ansiEscape(NonEmptyList.of(this.fg.fgMod, this.bg.bgMod)),
            ansiEscape(NonEmptyList.of(surroundings.fg.fgMod, surroundings.bg.bgMod)),
          ).some
        case (true, false) =>
          (
            ansiEscape(NonEmptyList.of(this.fg.fgMod)),
            ansiEscape(NonEmptyList.of(surroundings.fg.fgMod)),
          ).some
        case (false, true) =>
          (
            ansiEscape(NonEmptyList.of(this.bg.bgMod)),
            ansiEscape(NonEmptyList.of(surroundings.bg.bgMod)),
          ).some
        case (false, false) =>
          None
      }
    }

  }
  object ColorState {
    val Default: ColorState = ColorState(RawColor.Default, RawColor.Default)
  }

  // =====| ... |=====

  final case class Simple private[ColorString] (
      color: Color,
      str: String,
  ) extends ColorString {

    override def copy(cpF: Color => Color): ColorString =
      Simple(
        cpF(color),
        str,
      )

  }

  // color"someString${cString}${cString}someString${cString}someString"
  final case class Complex private[ColorString] (
      color: Color,
      pairs: List[(Option[String], ColorString)],
      tail: Option[String],
  ) extends ColorString {

    override def copy(cpF: Color => Color): ColorString =
      Complex(
        cpF(color),
        pairs,
        tail,
      )

  }

  trait Implicits {

    extension (obj: Any) {

      def toColorString: ColorString =
        Simple(Color.Empty, obj.toString)

    }

    implicit class ColorStringInterpolator(sc: StringContext) {

      def color(args: ColorString*): ColorString = {
        @tailrec
        def loop(
            sQueue: List[String],
            csQueue: List[ColorString],
            stack: List[(Option[String], ColorString)],
        ): (List[(Option[String], ColorString)], Option[String]) =
          (sQueue, csQueue) match {
            case (sH :: sT, csH :: csT) =>
              loop(
                sT,
                csT,
                (sH.toNES, csH) :: stack,
              )
            case (_, Nil) =>
              (stack.reverse, sQueue.mkString.toNES)
            case (Nil, csH :: csT) => // This should not be possible...
              loop(
                Nil,
                csT,
                (None, csH) :: stack,
              )
          }

        val (pairs, tail) = loop(sc.parts.toList, args.toList, Nil)

        ColorString.Complex(Color.Empty, pairs, tail)
      }

    }

    extension (csl: List[ColorString]) {

      def csMkString: ColorString =
        csMkString("", "", "")

      def csMkString(sep: String): ColorString =
        csMkString("", sep, "")

      def csMkString(start: String, sep: String, end: String): ColorString = {
        val sepO = sep.toNES

        val pairs: List[(Option[String], ColorString)] =
          csl.toNel match {
            case Some(csl) => csl.tail.foldLeft((start.toNES, csl.head) :: Nil) { (l, cs) => (sepO, cs) :: l }
            case None      => Nil
          }

        ColorString.Complex(Color.Empty, pairs, end.toNES)
      }

    }

  }
  object Implicits extends Implicits

}
