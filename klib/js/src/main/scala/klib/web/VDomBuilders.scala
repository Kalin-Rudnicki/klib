package klib.web

import org.scalajs.dom.*
import scala.scalajs.js

import klib.web.VDom.*

object VDomBuilders {
  export CSSAttrBuilders.*
  export KeyAttrBuilders.*
  export NodeElementBuilders.*
  export StdAttrBuilders.*

  object NodeElementBuilders {
    val body: NodeElement = NodeElement("body")
    val div: NodeElement = NodeElement("div")
    val span: NodeElement = NodeElement("span")
    val ul: NodeElement = NodeElement("ul")
    val ol: NodeElement = NodeElement("ol")
    val li: NodeElement = NodeElement("li")
    val br: NodeElement = NodeElement("br")
    val button: NodeElement = NodeElement("button")
    // TODO:
  }

  // ex: div.style(_) = _
  object CSSAttrBuilders {
    abstract class CSSAttrBuilder(scopedName: ScopedName) {
      def :=(value: String): CSSAttr = CSSAttr(scopedName, value)
    }
    abstract class ColorCSSAttrBuilder(scopedName: ScopedName) extends CSSAttrBuilder(scopedName) {
      def red: CSSAttr = this := "red"
      def blue: CSSAttr = this := "blue"
      def rgb(r: Int, g: Int, b: Int): CSSAttr = this := s"rgb($r, $g, $b)"
    }

    object color extends ColorCSSAttrBuilder("color")
    object backgroundColor extends ColorCSSAttrBuilder("background-color")
    object width extends CSSAttrBuilder("width")
    object height extends CSSAttrBuilder("height")
    object cursor extends CSSAttrBuilder("cursor")
    object userSelect extends CSSAttrBuilder("user-select")
    object padding extends CSSAttrBuilder("padding")
    object margin extends CSSAttrBuilder("margin")
    object display extends CSSAttrBuilder("display")
    object textAlign extends CSSAttrBuilder("text-align")
    // TODO:
  }

  // ex: div.setAttr(_, _)
  object StdAttrBuilders {
    abstract class StdAttrBuilder(scopedName: ScopedName) {
      def :=(value: String): StdAttr = StdAttr(scopedName, value)
    }

    // TODO:
  }

  // ex: div._ = _
  object KeyAttrBuilders {
    abstract class KeyAttrBuilder[T](name: String, convert: T => js.Any) {
      def :=(value: T): KeyAttr = KeyAttr(name, convert(value))
    }

    object onClick extends KeyAttrBuilder[MouseEvent => Unit]("onclick", identity(_))
    // TODO:
  }

}
