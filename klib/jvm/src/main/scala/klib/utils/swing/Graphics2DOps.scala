package klib.utils.swing

import java.awt.*

/**
  * Note : The default for graphics in java is that (0, 0) is at the top left of the screen.
  *      : All of these functions assume that you have modified this, so that (0, 0) is at the bottom left of the screen.
  *      : You can do this by calling the function `make (0, 0) bottomLeft`(getHeight)
  */
extension (self: Graphics2D) {

  def `make (0, 0) bottomLeft`(height: Int): Unit = {
    self.translate(0, height)
    self.scale(1, -1)
  }

  def stringWidth(string: String): Int =
    self.getFontMetrics.stringWidth(string)

  def drawStringCenteredWH(string: String, x0: Int, y0: Int, w: Int, h: Int): Unit = {
    val metrics = self.getFontMetrics
    val x = x0 + (w + metrics.stringWidth(string)) / 2
    val y = y0 + ((h - metrics.getHeight) / 2) - metrics.getAscent
    self.drawString(string, x, y)
  }
  def drawStringCenteredXH(string: String, x0: Int, y0: Int, x1: Int, h: Int): Unit = drawStringCenteredWH(string, x0, y0, x1 - x0, h)
  def drawStringCenteredWY(string: String, x0: Int, y0: Int, w: Int, y1: Int): Unit = drawStringCenteredWH(string, x0, y0, w, y1 - y0)
  def drawStringCenteredXY(string: String, x0: Int, y0: Int, x1: Int, y1: Int): Unit = drawStringCenteredWH(string, x0, y0, x1 - x0, y1 - y0)

  def fillRectWH(x0: Int, y0: Int, w: Int, h: Int): Unit =
    self.fillRect(x0, y0, w, h)
  def fillRectXH(x0: Int, y0: Int, x1: Int, h: Int): Unit = fillRectWH(x0, y0, x1 - x0, h)
  def fillRectWY(x0: Int, y0: Int, w: Int, y1: Int): Unit = fillRectWH(x0, y0, w, y1 - y0)
  def fillRectXY(x0: Int, y0: Int, x1: Int, y1: Int): Unit = fillRectWH(x0, y0, x1 - x0, y1 - y0)

}
