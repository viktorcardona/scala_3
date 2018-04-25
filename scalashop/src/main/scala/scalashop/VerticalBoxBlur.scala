package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

  /*
  original:
   def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
  */

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    //println("Vertical.from= "+from+", end= "+end+ ", radius= "+radius)
    //println("src.width= "+src.width+", src.height= "+src.height)
    var numPixels = 0
    var x = from
    while (x < end) {
      var y = 0
      while (y < src.height) {
        //println("x= "+x+", y= "+y)
        if (x >= 0 && x < src.width) {
          val blurPixel = boxBlurKernel(src, x, y, radius)
          //println("x= "+x+", y= "+y+" blurPixel: "+blurPixel)
          dst.update(x, y, blurPixel)
          //y += 1
          numPixels+=1
        }
        y += 1//new with if
      }
      x += 1
    }

    //println("Vertical. radius= "+radius+", from= "+from+", end= "+end+", src.width= "+src.width+", src.height= "+src.height+", width*height= "+(src.width*src.height)+", numPixels.Processed= "+numPixels)

  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method

      val number = Math.max( src.width / numTasks, 1)
      val strips = ( 0 until src.width ) by number

      val stripsOfTasks = strips.map(x => task{
        blur(src, dst, x, x + number, radius)
      })
      //strips.foreach(task => task.join())
      stripsOfTasks.map(task => task.join())

  }

}
