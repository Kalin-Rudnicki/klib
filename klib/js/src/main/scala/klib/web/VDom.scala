package klib.web

import cats.Monoid
import cats.syntax.option.*
import scala.annotation.tailrec
import scala.scalajs.js

import klib.utils.*

object VDom {

  final case class ScopedName(prefix: Option[String], name: String) {
    override def toString: String = prefix.fold(name)(p => s"$p:$name")
  }
  object ScopedName {
    def apply(prefix: String, name: String): ScopedName = ScopedName(prefix.some, name)
    def apply(name: String): ScopedName = ScopedName(None, name)
  }
  given Conversion[String, ScopedName] = ScopedName(_)

  sealed trait Modifier {

    def toBasics: List[Modifier.Basic] =
      this match {
        case basic: Modifier.Basic      => basic :: Nil
        case Modifier.Wrapped(children) => children
      }

  }
  object Modifier {
    sealed trait Basic extends Modifier
    final case class Wrapped(children: List[Basic]) extends Modifier

    def apply(children: Modifier*): Modifier =
      Wrapped(children.toList.flatMap(_.toBasics))

    def flatten(children: List[Modifier]): Modifier =
      Wrapped(children.flatMap(_.toBasics))

    val empty: Modifier = Wrapped(Nil)

    given Monoid[Modifier] = new Monoid[Modifier] {
      override def empty: Modifier = Modifier.empty
      override def combine(x: Modifier, y: Modifier): Modifier =
        Modifier(x, y)
    }
  }
  given Conversion[Unit, Modifier] = _ => Modifier.Wrapped(Nil)
  given Conversion[IterableOnce[Modifier], Modifier] = iter => Modifier(iter.toList*)

  sealed trait Element extends Modifier.Basic {
    def nodeName: String =
      this match {
        case NodeElement(tagName, _) => tagName.toUpperCase
        case _: TextElement          => "#text"
      }
  }

  final case class TextElement(text: String) extends Element
  given Conversion[String, TextElement] = TextElement(_)

  final case class NodeElement(
      tagName: String,
      reversedModifierLists: List[List[Modifier]],
  ) extends Element {

    def apply(modifiers: Modifier*): NodeElement = copy(reversedModifierLists = modifiers.toList :: reversedModifierLists)

    def modifiers: List[Modifier.Basic] = reversedModifierLists.reverse.flatten.flatMap(_.toBasics)

    def splitModifiers: (List[Element], Set[String], Map[ScopedName, String], Map[ScopedName, String], Map[String, js.Any]) = {
      val values =
        modifiers.foldLeft(
          (List.empty[Element], Set.empty[String], Map.empty[ScopedName, String], Map.empty[ScopedName, String], Map.empty[String, js.Any]),
        ) { (values, elem) =>
          elem match {
            case element: Element     => values.copy(_1 = element :: values._1)
            case className: ClassName => values.copy(_2 = className.classNames ++ values._2)
            case cssAttr: CSSAttr     => values.copy(_3 = values._3.updated(cssAttr.scopedName, cssAttr.value))
            case stdAttr: StdAttr     => values.copy(_4 = values._4.updated(stdAttr.scopedName, stdAttr.value))
            case keyAttr: KeyAttr     => values.copy(_5 = values._5.updated(keyAttr.name, keyAttr.value))
          }
        }

      values.copy(_1 = values._1.reverse)
    }

  }
  object NodeElement {
    def apply(nodeName: String): NodeElement = NodeElement(nodeName, Nil)
  }

  enum ClassName extends Modifier.Basic {
    case Block(block: String, modifiers: Set[String])
    case Element(block: String, element: String, modifiers: Set[String])

    def classNames: Set[String] =
      this match {
        case Block(block, modifiers)            => ClassName.classNames(block, modifiers)
        case Element(block, element, modifiers) => ClassName.classNames(s"${block}__$element", modifiers)
      }

  }
  object ClassName {

    private def classNames(base: String, modifiers: Set[String]): Set[String] =
      modifiers.map(modifier => s"$base--$modifier") + base

    def b(block: String): ClassName = Block(block, Set.empty)
    def b(block: String, m0: String, mN: String*): ClassName = Block(block, (m0 :: mN.toList).toSet)
    def b(block: String, m0: IterableOnce[String], mN: IterableOnce[String]*): ClassName = Block(block, (m0 :: mN.toList).toSet.flatten)

    def be(block: String, element: String): ClassName = Element(block, element, Set.empty)
    def be(block: String, element: String, m0: String, mN: String*): ClassName = Element(block, element, (m0 :: mN.toList).toSet)
    def be(block: String, element: String, m0: IterableOnce[String], mN: IterableOnce[String]*): ClassName = Element(block, element, (m0 :: mN.toList).toSet.flatten)

  }

  final case class CSSAttr(scopedName: ScopedName, value: String) extends Modifier.Basic
  final case class StdAttr(scopedName: ScopedName, value: String) extends Modifier.Basic
  final case class KeyAttr(name: String, value: js.Any) extends Modifier.Basic

}
