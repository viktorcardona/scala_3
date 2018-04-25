
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    def valX(newX: Int) = clamp(newX, 0, src.width - 1)
    def valY(newY: Int) = clamp(newY, 0, src.height - 1)
    def isValid(x1: Int, y1: Int) = (x1 >= 0 && y1 >= 0 && x1 < src.width && y1 < src.height)

    var rgbaSum = (0, 0, 0, 0)
    var numberOfPixels = 0

    def add(x1: Int, y1: Int): Unit =
      if (isValid(x1, y1)) {
        val pixel = src(valX(x1), valY(y1))
        rgbaSum = (rgbaSum._1 + red(pixel), rgbaSum._2 + green(pixel), rgbaSum._3 + blue(pixel), rgbaSum._4 + alpha(pixel))
        numberOfPixels += 1
      }

    var y1 = y - radius//from.topLeft
    while (y1 <= (y + radius)) {
      var x1 = x - radius//from.topLeft
      while (x1 <= (x + radius)) {
        add(x1, y1)
        x1 += 1
      }
      y1 += 1
    }

    rgba(
      rgbaSum._1 / numberOfPixels,
      rgbaSum._2 / numberOfPixels,
      rgbaSum._3 / numberOfPixels,
      rgbaSum._4 / numberOfPixels)
  }

  /*
2nd version: simple cleaner good code: but it is the wrost use of memory: so it is really bad code:

def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    def valX(newX: Int) = clamp(newX, 0, src.width - 1)
    def valY(newY: Int) = clamp(newY, 0, src.height - 1)
    def isValid(x1: Int, y1: Int) = (x1 >= 0 && y1 >= 0 && x1 < src.width && y1 < src.height)

    val pixels = (for (
      y1 <- y - radius to y + radius;
      x1 <- x - radius to x + radius
      if (isValid(x1, y1))
    ) yield src(valX(x1), valY(y1))).
      map( p => (red(p), green(p), blue(p), alpha(p)))

    rgba(
      pixels.map(_._1).sum / pixels.size,
      pixels.map(_._2).sum / pixels.size,
      pixels.map(_._3).sum / pixels.size,
      pixels.map(_._4).sum / pixels.size)
  }

1st working version after 20 tries:

 def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    def valX(newX: Int) = clamp(newX, 0, src.width  - 1)
    def valY(newY: Int) = clamp(newY, 0, src.height - 1)
    def isValid(x1:Int, y1:Int) = (x1 >= 0 && y1 >= 0 && x1 < src.width && y1 < src.height)
    if (!isValid(x, y)) {
      0
    } else if (radius < 1) {
      src(valX(x), valY(y))
    } else {
      val center = src(valX(x), valY(y))
      var pixel = (red(center), green(center), blue(center), alpha(center))
      var numberOfPixels = 1
      def merge(x1:Int, y1:Int): Unit =
        if (isValid(x1, y1)) {
          val pixel2 = src(valX(x1), valY(y1))
          pixel=(pixel._1+red(pixel2), pixel._2+green(pixel2), pixel._3+blue(pixel2), pixel._4+alpha(pixel2))
          numberOfPixels+=1
        }

      var radiusDelta = 1
      var numPixelsHorizontal = 3
      while (radiusDelta <= radius) {
        var count = 0
        val topLeftX = x - radiusDelta
        val topLeftY = y - radiusDelta
        val bottomRightX = x + radiusDelta
        val bottomRightY = y + radiusDelta
        while (count < numPixelsHorizontal) {
          merge(topLeftX + count, topLeftY)//Top Horizontal [left 2 right]
          merge(bottomRightX - count,bottomRightY)//Bottom Horizontal [right 2 left]
          if (count < numPixelsHorizontal - 2) {
            merge(topLeftX, topLeftY + count + 1)//Vertical Left [up 2 down]
            merge(bottomRightX, bottomRightY - count - 1)//Vertical Right [down 2 up]
          }
          count+=1
        }
        numPixelsHorizontal+=2
        radiusDelta+=1
      }
      rgba(
        pixel._1/ numberOfPixels,
        pixel._2/numberOfPixels,
        pixel._3/numberOfPixels,
        pixel._4/numberOfPixels)
    }
  }
  */
}
