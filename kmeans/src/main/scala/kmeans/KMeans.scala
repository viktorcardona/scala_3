package kmeans

import java.util.concurrent.ConcurrentSkipListSet

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random
import org.scalameter._
import common._

class KMeans {

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.size > 0)
    var minDistance = p.squareDistance(means(0))
    var closest = means(0)
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    if (means.isEmpty) GenMap()
    else if (points.isEmpty) means.map(mean => (mean, GenSeq())).toMap
    else points.groupBy(point => findClosest(point, means))
  }

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.length == 0) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    var meansUpdated: GenSeq[Point] = mutable.Seq[Point]()
    for (p <- oldMeans) meansUpdated = meansUpdated:+findAverage(p, classified(p))
    meansUpdated
  }

  /*
  1st_version
def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    var meansUpdated: GenSeq[Point] = GenSeq()
    var i = 0
    while (i < oldMeans.length) {
      val pointOldMean = oldMeans(i)
      val points:GenSeq[Point] = classified(pointOldMean)
      val pointNewMean = findAverage(pointOldMean, points)
      meansUpdated = meansUpdated:+ pointNewMean
      i+=1
    }
    meansUpdated
  }

  2nd version:
  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    var meansUpdated: GenSeq[Point] = mutable.Seq[Point]()
    for (p <- oldMeans) meansUpdated = meansUpdated:+findAverage(p, classified(p))
    meansUpdated
  }
  */

  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    //if (oldMeans.isEmpty || newMeans.isEmpty || oldMeans.length != newMeans.length)
    if (oldMeans.length != newMeans.length)
      false
    else {
      var i = 0
      var converge = true
      while (i < oldMeans.length) {
        val distance = oldMeans(i).squareDistance(newMeans(i))
        if (distance > eta) {
          converge = false
        }
        i+=1
      }
      converge
    }
  }

  /*
step2
Each of the means was significantly updated. This is a good indication that the algorithm did
  not yet converge, so we repeat the steps again -- we first classify all the points:

step3
And then we update the means again:


def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]]
def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point]
def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean

*/
  @tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val classified: GenMap[Point, GenSeq[Point]] = classify(points, means)
    val newMeans: GenSeq[Point] = update(classified, means)
    if (!converged(eta)(means, newMeans)) kMeans(points, newMeans, eta) else newMeans // your implementation need to be tail recursive
  }
  /*
origen:
@tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    if (???) kMeans(???, ???, ???) else ??? // your implementation need to be tail recursive
  }
  */
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}
