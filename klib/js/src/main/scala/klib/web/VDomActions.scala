package klib.web

import cats.syntax.option.*
import org.scalajs.dom.{document, window, Element as DomElement, HTMLHtmlElement, Node}
import scala.annotation.tailrec
import scala.scalajs.js.JSON

import klib.utils.{given, *}
import klib.web.ModifierBuilders.NodeElementBuilders
import klib.web.VDom.*

object VDomActions {

  private def build(element: Element): Node =
    element match {
      case nodeElement: NodeElement =>
        val node = document.createElement(nodeElement.tagName)
        val (elements, classNames, cssAttrs, stdAttrs, keyAttrs) = nodeElement.splitModifiers

        setClassNames(node, classNames)
        setStyle(node, cssAttrs)
        stdAttrs.foreach { (name, value) => node.setAttribute(name.toString, value) }
        // TODO: keyAttrs

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

  def diffElements(
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
                val (newElements, newClassNames, newCSSAttrs, newStdAttrs, newKeyAttrs) = newNodeElement.splitModifiers
                val (oldElements, oldClassNames, oldCSSAttrs, oldStdAttrs, oldKeyAttrs) = oldNodeElement.splitModifiers

                if (newClassNames != oldClassNames)
                  setClassNames(node, newClassNames)
                if (newCSSAttrs != oldCSSAttrs)
                  setStyle(node, newCSSAttrs)
                // TODO: keyAttrs

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

  def setBody(elements: List[Element]): Unit =
    document.body = build(NodeElementBuilders.body(elements*)).asInstanceOf[HTMLHtmlElement]

  final class BodyTracker {

    private var oldVDoms: Option[List[Element]] = None

    def render(newVDoms: List[Element]): Unit = {
      oldVDoms match {
        case Some(oldVDoms) => diffElements(document.body, newVDoms, oldVDoms)
        case None           => setBody(newVDoms)
      }
      oldVDoms = newVDoms.some
    }

  }

}

def main(args: Array[String]): Unit = {
  import org.scalajs.dom.console
  import ModifierBuilders.*

  val bodyTracker = VDomActions.BodyTracker()

  def render(elements: Element*): Unit = {
    bodyTracker.render(elements.toList)
  }

  def rec(idx: Int): Unit = {
    render(
      if (true)
        ul(
          (1 to idx).map { i =>
            if (i == idx) li(color.blue, "[", i.toString, "]")
            else li(i.toString)
          } *,
        )
      else
        ul,
      div(
        height := "50px",
        backgroundColor.red,
      ),
    )
    if (idx < 20)
      window.setTimeout(
        { () =>
          rec(idx + 1)
        },
        500,
      )
  }

  rec(0)

}
