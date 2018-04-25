package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  /*
  Implement the sequential lineOfSight method, which, for each height entry in the input array
  (except for input(0) which is the location of the observer and is always zero), writes the
  maximum angle until that point into the output array (output(0) should be 0)

  Note that what we call an angle in this assignment is actually the tangent of the angle.
  Indeed, xs(i) is the opposing side of the angle and i the adjacent side.
  The ratio xs(i) / i that you compute is in fact the tangent of the angle!
   */
  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    output(0)=0
    var distance = 1
    while (distance < input.length) {
      output(distance) = max(input(distance)/distance, output(distance-1))
      distance+=1
    }
  }
  /*
  When we see a sequential algorithm that produces a sequence of values by
    traversing the input from left to right, this is an indication that
    the algorithm might have a parallel prefix sum variant.
    So let's try to implement one!
   */

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var maxAngle = input(from)
    if (from != 0) maxAngle = maxAngle/from
    var distance = from + 1
    while (distance < until) {
      maxAngle = max(input(distance)/distance, maxAngle)
      distance+=1
    }
    maxAngle
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if ((end - from) <= threshold)///////   <=
      Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val mid = from + ((end - from)/2)
      val (left, right) = parallel(upsweep(input, from, mid, threshold),
                                   upsweep(input,  mid, end, threshold))
      Node(left, right)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    if (from >= until) return

    var distance = from
    if (from == 0) {
      output(distance) = 0
    } else {
      output(distance) = max(input(distance) / distance, startingAngle)
    }
    distance += 1
    while (distance < until) {
      output(distance) = max(input(distance) / distance, output(distance - 1))
      distance += 1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = tree match {
    case Leaf(from, until, maxPrevious) => downsweepSequential(input, output, startingAngle, from, until)
    case Node(left, right) => parallel(downsweep(input, output, max(left.maxPrevious, startingAngle), left),
                                       downsweep(input, output, max(left.maxPrevious, startingAngle), right))
  }

  /** Compute the line-of-sight in parallel.
    Finally, implement parLineOfSight using the upsweep and downsweep methods
    */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    output(0)=0
    val tree = upsweep(input, 1, input.length, threshold)
    downsweep(input, output, 0, tree)
  }

  private def show(t: Tree):Unit = t match {
    case Leaf(from, until, maxPrevious) => println("from:"+from+", until:"+until+", maxPrev:"+maxPrevious)
    case Node(left, right) => println("There are nodes....")
                              show(left)
                              show(right)
  }
}
