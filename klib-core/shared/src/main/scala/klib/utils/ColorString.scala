package klib.utils

import scala.annotation.tailrec

import klib.fp.extensions.{given, _}
import klib.fp.types.Maybe.instances.{given, _}
import klib.fp.types._

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
        ColorString.Complex(color, pairs, tail.cata(_ + otherStr, otherStr).some)
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
          oStr.cata(_.length, 0) + cStr.length
        }.sum + tail.cata(_.length, 0)
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
            str.foreach(stringBuilder.append(_))
            rec(cStr)
          }
          tail.foreach(stringBuilder.append(_))
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
  import klib.utils.{Color => RawColor}

  private def ansiEscape(codes: NonEmptyList[String]): String =
    s"\u001b[${codes.toList.mkString(";")}m"

  final case class Color(
      fg: Maybe[RawColor],
      bg: Maybe[RawColor],
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
    def diffWithState(colorState: ColorState): Maybe[(String, ColorState)] =
      (fg.filterNot(_ == colorState.fg), bg.filterNot(_ == colorState.bg)) match {
        case (Some(fg), Some(bg)) =>
          (
            ansiEscape(NonEmptyList.nel(fg.fgMod, bg.bgMod)),
            colorState.copy(fg = fg, bg = bg),
          ).some
        case (Some(fg), None) =>
          (
            ansiEscape(NonEmptyList.nel(fg.fgMod)),
            colorState.copy(fg = fg),
          ).some
        case (None, Some(bg)) =>
          (
            ansiEscape(NonEmptyList.nel(bg.bgMod)),
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

    def colorizeAndDeColorize(surroundings: ColorState): Maybe[(String, String)] = {
      (this.fg != surroundings.fg, this.bg != surroundings.bg) match {
        case (true, true) =>
          (
            ansiEscape(NonEmptyList.nel(this.fg.fgMod, this.bg.bgMod)),
            ansiEscape(NonEmptyList.nel(surroundings.fg.fgMod, surroundings.bg.bgMod)),
          ).some
        case (true, false) =>
          (
            ansiEscape(NonEmptyList.nel(this.fg.fgMod)),
            ansiEscape(NonEmptyList.nel(surroundings.fg.fgMod)),
          ).some
        case (false, true) =>
          (
            ansiEscape(NonEmptyList.nel(this.bg.bgMod)),
            ansiEscape(NonEmptyList.nel(surroundings.bg.bgMod)),
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
      pairs: List[(Maybe[String], ColorString)],
      tail: Maybe[String],
  ) extends ColorString {

    override def copy(cpF: Color => Color): ColorString =
      Complex(
        cpF(color),
        pairs,
        tail,
      )

  }

  object extensions {

    extension (obj: Any) {

      def toColorString: ColorString =
        Simple(Color.Empty, obj.toString)

    }

    extension (str: String) {

      def stripColor: String =
        str.replaceAll("\u001b\\[\\d+(;\\d+)*m", "")

      def replaceColor(regex: String, color: RawColor): String =
        str.replaceAll(regex, s"\u001b[${color.fgMod}m$$0\u001b[0m")

    }

    extension (csl: List[ColorString]) {

      def csMkString: ColorString =
        csMkString("")

      def csMkString(joinStr: String, color: Color = Color.Empty): ColorString = {
        val js = joinStr.nonEmpty ? joinStr.some | None

        val pairs: List[(Maybe[String], ColorString)] = {
          @tailrec
          def loop(
              queue: List[ColorString],
              stack: List[(Maybe[String], ColorString)],
          ): List[(Maybe[String], ColorString)] =
            queue match {
              case Nil =>
                stack.reverse
              case head :: tail =>
                loop(
                  tail,
                  (js, head) :: stack,
                )
            }

          csl match {
            case Nil =>
              Nil
            case head :: tail =>
              loop(tail, (None, head) :: Nil)
          }
        }

        ColorString.Complex(color, pairs, None)
      }

    }

    implicit class ColorStringInterpolator(sc: StringContext) {

      def color(args: ColorString*): ColorString = {
        def nonEmptyString(str: String): Maybe[String] =
          str.nonEmpty ? str.some | None

        @tailrec
        def loop(
            sQueue: List[String],
            csQueue: List[ColorString],
            stack: List[(Maybe[String], ColorString)],
        ): (List[(Maybe[String], ColorString)], Maybe[String]) =
          (sQueue, csQueue) match {
            case (sH :: sT, csH :: csT) =>
              loop(
                sT,
                csT,
                (nonEmptyString(sH), csH) :: stack,
              )
            case (_, Nil) =>
              (stack.reverse, nonEmptyString(sQueue.mkString))
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

  }

}
