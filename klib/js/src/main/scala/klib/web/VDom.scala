package klib.web

import cats.syntax.option.*
import scala.annotation.tailrec

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

    def toIdtStr: IndentedString = {
      import IndentedString.*
      this match {
        case textElement: TextElement => textElement.text.unesc
        case nodeElement: NodeElement =>
          val (elements, modifiers) = nodeElement.modifiers.partition(_.isInstanceOf[Element])

          `inline`(
            nodeElement.tagName,
            indented(
              modifiers.map(_.toIdtStr),
              indented(elements.map(_.toIdtStr)),
            ),
          )
        case className: ClassName => s"[className] ${className.classNames.mkString(" ")}"
        case cssAttr: CSSAttr     => s"[cssAttr] ${cssAttr.scopedName} => ${cssAttr.value}"
        case stdAttr: StdAttr     => s"[stdAttr] ${stdAttr.scopedName} => ${stdAttr.value}"
        case keyAttr: KeyAttr     => s"[keyAttr] ${keyAttr.name} => ${keyAttr.value}"
      }
    }

  }
  sealed trait Element extends Modifier {
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

    def modifiers: List[Modifier] = reversedModifierLists.reverse.flatten

    def splitModifiers: (List[Element], Set[String], Map[ScopedName, String], Map[ScopedName, String], Map[String, Any]) = {
      val values =
        modifiers.foldLeft(
          (List.empty[Element], Set.empty[String], Map.empty[ScopedName, String], Map.empty[ScopedName, String], Map.empty[String, Any]),
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

  enum ClassName extends Modifier {
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

    def b(block: String, modifiers: String*): ClassName = Block(block, modifiers.toSet)
    def be(block: String, element: String, modifiers: String*): ClassName = Element(block, element, modifiers.toSet)

  }

  final case class CSSAttr(scopedName: ScopedName, value: String) extends Modifier
  final case class StdAttr(scopedName: ScopedName, value: String) extends Modifier
  final case class KeyAttr(name: String, value: Any) extends Modifier

}
