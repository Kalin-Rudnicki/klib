package klib.web

import cats.syntax.option.*
import org.scalajs.dom.{document, window, Element as DomElement, HTMLHtmlElement, Node}
import scala.annotation.tailrec
import scala.scalajs.js.JSON
import scalajs.js.Dynamic
import zio.*

import klib.utils.{given, *}
import klib.web.VDom.*
import klib.web.VDomBuilders.NodeElementBuilders

object VDomActions {

  private def build(element: Element): Node =
    element match {
      case nodeElement: NodeElement =>
        val node = document.createElement(nodeElement.tagName)
        val nodeDynamic = node.asInstanceOf[Dynamic]
        val (elements, classNames, cssAttrs, stdAttrs, keyAttrs) = nodeElement.splitModifiers

        setClassNames(node, classNames)
        setStyle(node, cssAttrs)
        stdAttrs.foreach { (name, value) => node.setAttribute(name.toString, value) }
        keyAttrs.foreach { (key, value) =>
          nodeDynamic.updateDynamic(key)(value)
        }

        elements.foreach(element => node.appendChild(build(element)))

        node
      case textElement: TextElement =>
        document.createTextNode(textElement.text)
    }

  private def setClassNames(element: DomElement, classNames: Set[String]): Unit =
    if (classNames.nonEmpty) element.setAttribute("class", classNames.mkString(" "))
    else element.removeAttribute("class")

  private def setStyle(element: DomElement, cssAttrs: Map[ScopedName, String]): Unit =
    if (cssAttrs.nonEmpty) element.setAttribute("style", cssAttrs.toList.map { (sn, v) => s"$sn: $v" }.mkString("; "))
    else element.removeAttribute("style")

  private def diffElements(
      parent: Node,
      newVDoms: List[Element],
      oldVDoms: List[Element],
  ): Unit = {
    val children = (0 until parent.childNodes.length).map(parent.childNodes(_)).toArray

    @tailrec
    def loop(
        newVDoms: List[Element],
        oldVDoms: List[Element],
        idx: Int,
    ): Unit =
      (newVDoms, oldVDoms) match {
        case (newVDom :: newVDomTail, oldVDom :: oldVDomTail) =>
          (newVDom, oldVDom) match {
            case (newNodeElement: NodeElement, oldNodeElement: NodeElement) =>
              if (newNodeElement.tagName != oldNodeElement.tagName) parent.replaceChild(build(newNodeElement), children(idx))
              else {
                val node = children(idx).asInstanceOf[DomElement]
                val nodeDynamic = node.asInstanceOf[Dynamic]
                val (newElements, newClassNames, newCSSAttrs, newStdAttrs, newKeyAttrs) = newNodeElement.splitModifiers
                val (oldElements, oldClassNames, oldCSSAttrs, oldStdAttrs, oldKeyAttrs) = oldNodeElement.splitModifiers

                if (newClassNames != oldClassNames)
                  setClassNames(node, newClassNames)
                if (newCSSAttrs != oldCSSAttrs)
                  setStyle(node, newCSSAttrs)
                (newKeyAttrs.keySet ++ oldKeyAttrs.keySet).foreach { key =>
                  (newKeyAttrs.get(key), oldKeyAttrs.get(key)) match {
                    case (Some(newValue), Some(oldValue)) =>
                      if (newValue != oldValue)
                        nodeDynamic.updateDynamic(key)(newValue)
                    case (Some(newValue), None) =>
                      nodeDynamic.updateDynamic(key)(newValue)
                    case (None, Some(_)) =>
                      nodeDynamic.updateDynamic(key)(null)
                    case (None, None) =>
                  }
                }

                diffElements(node, newElements, oldElements)
              }
            case (newTextElement: TextElement, oldTextElement: TextElement) =>
              if (newTextElement.text != oldTextElement.text)
                parent.replaceChild(build(newTextElement), children(idx))
            case (newVDom, _) =>
              parent.replaceChild(build(newVDom), children(idx))
          }
          loop(newVDomTail, oldVDomTail, idx + 1)
        case (Nil, _ :: oldVDomTail) =>
          parent.removeChild(children(idx))
          loop(Nil, oldVDomTail, idx + 1)
        case (newVDom :: newVDomTail, Nil) =>
          parent.appendChild(build(newVDom))
          loop(newVDomTail, Nil, idx + 1)
        case (Nil, Nil) =>
      }

    loop(newVDoms, oldVDoms, 0)
  }

  private def setBody(elements: List[Element]): Unit =
    document.body = build(NodeElementBuilders.body(elements*)).asInstanceOf[HTMLHtmlElement]

  final class Renderer private (ref: Ref.Synchronized[Option[List[Element]]]) {

    final def setPageTitle(title: String): TaskM[Unit] =
      ZIOM.attempt(window.document.title = title)

    def render(newVDoms: List[Element]): TaskM[Unit] =
      ref.updateZIO { oldVDoms =>
        ZIOM
          .attempt {
            oldVDoms match {
              case Some(oldVDoms) => diffElements(document.body, newVDoms, oldVDoms)
              case None           => setBody(newVDoms)
            }
          }
          .as(newVDoms.some)
      }

  }
  object Renderer {

    val Initial: UIO[Renderer] =
      Ref.Synchronized.make(Option.empty[List[Element]]).map(Renderer(_))

  }

}
