package klib.web

import klib.web.VDom.*

object ModifierBuilders {
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
    // TODO:
  }

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
    // TODO:
  }

  object StdAttrBuilders {
    abstract class StdAttrBuilder(scopedName: ScopedName) {
      def :=(value: String): StdAttr = StdAttr(scopedName, value)
    }

    // TODO:
  }

  object KeyAttrBuilders {
    abstract class KeyAttrBuilder[T](name: String) {
      def :=(value: T): KeyAttr = KeyAttr(name, value)
    }

    object onClick extends KeyAttrBuilder[Unit => Unit]("onClick")
    // TODO:
  }

}
