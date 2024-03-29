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
    val h1: NodeElement = NodeElement("h1")
    val h2: NodeElement = NodeElement("h2")
    val h3: NodeElement = NodeElement("h3")
    val h4: NodeElement = NodeElement("h4")
    val h5: NodeElement = NodeElement("h5")
    val h6: NodeElement = NodeElement("h6")
    val p: NodeElement = NodeElement("p")
    val input: NodeElement = NodeElement("input")
    val textarea: NodeElement = NodeElement("textarea")
    val a: NodeElement = NodeElement("a")
    val img: NodeElement = NodeElement("img")
    val table: NodeElement = NodeElement("table")
    val tr: NodeElement = NodeElement("tr")
    val th: NodeElement = NodeElement("th")
    val td: NodeElement = NodeElement("td")
    val form: NodeElement = NodeElement("form")
    val label: NodeElement = NodeElement("label")
    val select: NodeElement = NodeElement("select")
    val option: NodeElement = NodeElement("option")
    val optgroup: NodeElement = NodeElement("optgroup")
    val fieldset: NodeElement = NodeElement("fieldset")
    val legend: NodeElement = NodeElement("legend")
    val details: NodeElement = NodeElement("details")
    val summary: NodeElement = NodeElement("summary")
    val menu: NodeElement = NodeElement("menu")
    val menuItem: NodeElement = NodeElement("menuItem")
    val menuList: NodeElement = NodeElement("menuList")
    val dl: NodeElement = NodeElement("dl")
    val dt: NodeElement = NodeElement("dt")
    val dd: NodeElement = NodeElement("dd")
    val article: NodeElement = NodeElement("article")
    val section: NodeElement = NodeElement("section")
    val header: NodeElement = NodeElement("header")
    val footer: NodeElement = NodeElement("footer")
    val nav: NodeElement = NodeElement("nav")
    val main: NodeElement = NodeElement("main")
    val aside: NodeElement = NodeElement("aside")
    val figure: NodeElement = NodeElement("figure")
    val figcaption: NodeElement = NodeElement("figcaption")
    val time: NodeElement = NodeElement("time")
    val mark: NodeElement = NodeElement("mark")
    val ruby: NodeElement = NodeElement("ruby")
    val rt: NodeElement = NodeElement("rt")
    val rp: NodeElement = NodeElement("rp")
    val bdi: NodeElement = NodeElement("bdi")
    val bdo: NodeElement = NodeElement("bdo")
    val wbr: NodeElement = NodeElement("wbr")
    val ins: NodeElement = NodeElement("ins")
    val del: NodeElement = NodeElement("del")
    val caption: NodeElement = NodeElement("caption")
    val colgroup: NodeElement = NodeElement("colgroup")
    val col: NodeElement = NodeElement("col")
    val tbody: NodeElement = NodeElement("tbody")
    val thead: NodeElement = NodeElement("thead")
    val tfoot: NodeElement = NodeElement("tfoot")
    val iframe: NodeElement = NodeElement("iframe")
    val embed: NodeElement = NodeElement("embed")
    val param: NodeElement = NodeElement("param")
    val video: NodeElement = NodeElement("video")
    val audio: NodeElement = NodeElement("audio")
    val source: NodeElement = NodeElement("source")
    val track: NodeElement = NodeElement("track")
    val canvas: NodeElement = NodeElement("canvas")
    val svg: NodeElement = NodeElement("svg")
    val g: NodeElement = NodeElement("g")
    val defs: NodeElement = NodeElement("defs")
    val symbol: NodeElement = NodeElement("symbol")
    // TODO: more?
  }

  // ex: div.style(_) = _
  object CSSAttrBuilders {
    abstract class CSSAttrBuilder(scopedName: ScopedName) {
      final def :=(value: String): CSSAttr = CSSAttr(scopedName, value)

      final def inherit: CSSAttr = this := "inherit"
      final def initial: CSSAttr = this := "initial"
      final def unset: CSSAttr = this := "unset"
      final def revert: CSSAttr = this := "revert"
    }
    abstract class ColorCSSAttrBuilder(scopedName: ScopedName) extends CSSAttrBuilder(scopedName) {
      def red: CSSAttr = this := "red"
      def blue: CSSAttr = this := "blue"
      def green: CSSAttr = this := "green"
      def black: CSSAttr = this := "black"
      def white: CSSAttr = this := "white"
      def gray: CSSAttr = this := "gray"
      def yellow: CSSAttr = this := "yellow"
      def orange: CSSAttr = this := "orange"
      def purple: CSSAttr = this := "purple"
      def brown: CSSAttr = this := "brown"
      def pink: CSSAttr = this := "pink"
      def cyan: CSSAttr = this := "cyan"
      def magenta: CSSAttr = this := "magenta"
      def transparent: CSSAttr = this := "transparent"
      def silver: CSSAttr = this := "silver"
      def lime: CSSAttr = this := "lime"
      def maroon: CSSAttr = this := "maroon"
      def olive: CSSAttr = this := "olive"
      def teal: CSSAttr = this := "teal"
      def navy: CSSAttr = this := "navy"
      def fuchsia: CSSAttr = this := "fuchsia"
      def aqua: CSSAttr = this := "aqua"
      def rgb(r: Int, g: Int, b: Int): CSSAttr = this := s"rgb($r, $g, $b)"
      // TODO: more?
    }

    object color extends ColorCSSAttrBuilder("color")
    object backgroundColor extends ColorCSSAttrBuilder("background-color")
    object width extends CSSAttrBuilder("width")
    object height extends CSSAttrBuilder("height")
    object cursor extends CSSAttrBuilder("cursor") {
      def pointer: CSSAttr = this := "pointer"
      def auto: CSSAttr = this := "auto"
      def crosshair: CSSAttr = this := "crosshair"
      def default: CSSAttr = this := "default"
      def move: CSSAttr = this := "move"
      def text: CSSAttr = this := "text"
      def help: CSSAttr = this := "help"
      def progress: CSSAttr = this := "progress"
      def noDrop: CSSAttr = this := "noDrop"
      def notAllowed: CSSAttr = this := "notAllowed"
      def eResize: CSSAttr = this := "eResize"
      def nResize: CSSAttr = this := "nResize"
      def neResize: CSSAttr = this := "neResize"
      def nwResize: CSSAttr = this := "nwResize"
      def sResize: CSSAttr = this := "sResize"
      def seResize: CSSAttr = this := "seResize"
      def swResize: CSSAttr = this := "swResize"
      def wResize: CSSAttr = this := "wResize"
      def ewResize: CSSAttr = this := "ewResize"
      def nsResize: CSSAttr = this := "nsResize"
      def neswResize: CSSAttr = this := "neswResize"
      def nwseResize: CSSAttr = this := "nwseResize"
      def colResize: CSSAttr = this := "colResize"
      def rowResize: CSSAttr = this := "rowResize"
      def allScroll: CSSAttr = this := "allScroll"
      def zoomIn: CSSAttr = this := "zoomIn"
      def zoomOut: CSSAttr = this := "zoomOut"
    }
    object userSelect extends CSSAttrBuilder("user-select") {
      def none: CSSAttr = this := "none"
      def text: CSSAttr = this := "text"
      def all: CSSAttr = this := "all"
      def auto: CSSAttr = this := "auto"
      def contain: CSSAttr = this := "contain"
      def allScroll: CSSAttr = this := "allScroll"
      def element: CSSAttr = this := "element"
      def elements: CSSAttr = this := "elements"
      def toggle: CSSAttr = this := "toggle"
    }
    object padding extends CSSAttrBuilder("padding")
    object paddingTop extends CSSAttrBuilder("padding-top")
    object paddingRight extends CSSAttrBuilder("padding-right")
    object paddingBottom extends CSSAttrBuilder("padding-bottom")
    object paddingLeft extends CSSAttrBuilder("padding-left")
    object margin extends CSSAttrBuilder("margin")
    object marginTop extends CSSAttrBuilder("margin-top")
    object marginRight extends CSSAttrBuilder("margin-right")
    object marginBottom extends CSSAttrBuilder("margin-bottom")
    object marginLeft extends CSSAttrBuilder("margin-left")
    object display extends CSSAttrBuilder("display") {
      def inline: CSSAttr = this := "inline"
      def block: CSSAttr = this := "block"
      def inlineBlock: CSSAttr = this := "inline-block"
      def flex: CSSAttr = this := "flex"
      def inlineFlex: CSSAttr = this := "inline-flex"
      def table: CSSAttr = this := "table"
      def tableCell: CSSAttr = this := "table-cell"
      def tableRow: CSSAttr = this := "table-row"
      def tableRowGroup: CSSAttr = this := "table-row-group"
      def tableHeaderGroup: CSSAttr = this := "table-header-group"
      def tableFooterGroup: CSSAttr = this := "table-footer-group"
      def tableColumn: CSSAttr = this := "table-column"
      def tableColumnGroup: CSSAttr = this := "table-column-group"
      def tableCaption: CSSAttr = this := "table-caption"
      def none: CSSAttr = this := "none"
    }
    object textAlign extends CSSAttrBuilder("text-align")
    object verticalAlign extends CSSAttrBuilder("vertical-align")
    object fontSize extends CSSAttrBuilder("font-size")
    object fontWeight extends CSSAttrBuilder("font-weight")
    object fontFamily extends CSSAttrBuilder("font-family")
    object borderRadius extends CSSAttrBuilder("border-radius")
    object borderWidth extends CSSAttrBuilder("border-width")
    object borderStyle extends CSSAttrBuilder("border-style")
    object borderColor extends ColorCSSAttrBuilder("border-color")
    object borderTop extends CSSAttrBuilder("border-top")
    object borderRight extends CSSAttrBuilder("border-right")
    object borderBottom extends CSSAttrBuilder("border-bottom")
    object borderLeft extends CSSAttrBuilder("border-left")
    object border extends CSSAttrBuilder("border")
    object whiteSpace extends CSSAttrBuilder("white-space") {
      def normal: CSSAttr = this := "normal"
      def noWrap: CSSAttr = this := "nowrap"
      def pre: CSSAttr = this := "pre"
      def preWrap: CSSAttr = this := "pre-wrap"
      def preLine: CSSAttr = this := "pre-line"
      def breakSpaces: CSSAttr = this := "break-spaces"
    }
    object boxShadow extends CSSAttrBuilder("box-shadow")
    object textShadow extends CSSAttrBuilder("text-shadow")
    object transform extends CSSAttrBuilder("transform")
    object opacity extends CSSAttrBuilder("opacity")
    object visibility extends CSSAttrBuilder("visibility")
    object zIndex extends CSSAttrBuilder("z-index")
    object overflow extends CSSAttrBuilder("overflow")
    object overflowX extends CSSAttrBuilder("overflow-x")
    object overflowY extends CSSAttrBuilder("overflow-y")
    object transformOrigin extends CSSAttrBuilder("transform-origin")
    object animationName extends CSSAttrBuilder("animation-name")
    object animationDuration extends CSSAttrBuilder("animation-duration")
    object animationTimingFunction extends CSSAttrBuilder("animation-timing-function")
    object animationDelay extends CSSAttrBuilder("animation-delay")
    object animationDirection extends CSSAttrBuilder("animation-direction")
    object animationIterationCount extends CSSAttrBuilder("animation-iteration-count")
    object animationFillMode extends CSSAttrBuilder("animation-fill-mode")
    object animationPlayState extends CSSAttrBuilder("animation-play-state")
    // TODO: more?
  }

  // ex: div.setAttr(_, _)
  object StdAttrBuilders {
    abstract class StdAttrBuilder(scopedName: ScopedName) {
      final def :=(value: String): StdAttr = StdAttr(scopedName, value)
    }

    object `type` extends StdAttrBuilder("type") {
      def text: StdAttr = this := "text"
      def number: StdAttr = this := "number"
      def password: StdAttr = this := "password"
      def checkbox: StdAttr = this := "checkbox"
      def radio: StdAttr = this := "radio"
      def submit: StdAttr = this := "submit"
      def reset: StdAttr = this := "reset"
      def button: StdAttr = this := "button"
      def hidden: StdAttr = this := "hidden"
      def image: StdAttr = this := "image"
      def file: StdAttr = this := "file"
      def color: StdAttr = this := "color"
      def date: StdAttr = this := "date"
      def datetime: StdAttr = this := "datetime"
      def datetimeLocal: StdAttr = this := "datetime-local"
      def email: StdAttr = this := "email"
      def month: StdAttr = this := "month"
      def range: StdAttr = this := "range"
      def search: StdAttr = this := "search"
      def tel: StdAttr = this := "tel"
      def time: StdAttr = this := "time"
      def url: StdAttr = this := "url"
      def week: StdAttr = this := "week"
      def inherit: StdAttr = this := "inherit"
      def initial: StdAttr = this := "initial"
      def unset: StdAttr = this := "unset"
    }
    // TODO:
  }

  // ex: div._ = _
  object KeyAttrBuilders {
    abstract class KeyAttrBuilder[T](name: String, convert: T => js.Any) {
      final def :=(value: T): KeyAttr = KeyAttr(name, convert(value))
    }

    object onClick extends KeyAttrBuilder[MouseEvent => Unit]("onclick", identity(_))
    object onDblClick extends KeyAttrBuilder[MouseEvent => Unit]("ondblclick", identity(_))
    object onContextMenu extends KeyAttrBuilder[MouseEvent => Unit]("oncontextmenu", identity(_))
    object onMouseDown extends KeyAttrBuilder[MouseEvent => Unit]("onmousedown", identity(_))
    object onMouseUp extends KeyAttrBuilder[MouseEvent => Unit]("onmouseup", identity(_))
    object onMouseOver extends KeyAttrBuilder[MouseEvent => Unit]("onmouseover", identity(_))
    object onMouseMove extends KeyAttrBuilder[MouseEvent => Unit]("onmousemove", identity(_))
    object onMouseOut extends KeyAttrBuilder[MouseEvent => Unit]("onmouseout", identity(_))
    object onMouseEnter extends KeyAttrBuilder[MouseEvent => Unit]("onmouseenter", identity(_))
    object onMouseLeave extends KeyAttrBuilder[MouseEvent => Unit]("onmouseleave", identity(_))
    object onChange extends KeyAttrBuilder[Event => Unit]("onchange", identity(_))
    object onKeyDown extends KeyAttrBuilder[KeyboardEvent => Unit]("onkeydown", identity(_))
    object onKeyUp extends KeyAttrBuilder[KeyboardEvent => Unit]("onkeyup", identity(_))
    object onKeyPress extends KeyAttrBuilder[KeyboardEvent => Unit]("onkeypress", identity(_))
    object onSubmit extends KeyAttrBuilder[Event => Unit]("onsubmit", identity(_))
    object onReset extends KeyAttrBuilder[Event => Unit]("onreset", identity(_))
    object onFocus extends KeyAttrBuilder[FocusEvent => Unit]("onfocus", identity(_))
    object onBlur extends KeyAttrBuilder[FocusEvent => Unit]("onblur", identity(_))
    object onDrag extends KeyAttrBuilder[DragEvent => Unit]("ondrag", identity(_))
    object onDragStart extends KeyAttrBuilder[DragEvent => Unit]("ondragstart", identity(_))
    object onDragEnd extends KeyAttrBuilder[DragEvent => Unit]("ondragend", identity(_))
    object onDragEnter extends KeyAttrBuilder[DragEvent => Unit]("ondragenter", identity(_))
    object onDragLeave extends KeyAttrBuilder[DragEvent => Unit]("ondragleave", identity(_))
    object onDragOver extends KeyAttrBuilder[DragEvent => Unit]("ondragover", identity(_))
    object onDrop extends KeyAttrBuilder[DragEvent => Unit]("ondrop", identity(_))

    object value extends KeyAttrBuilder[String]("value", identity(_))
    // TODO: more?
  }

}
