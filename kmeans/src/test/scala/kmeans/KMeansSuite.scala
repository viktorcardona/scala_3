package kmeans

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._

object KM extends KMeans
import KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal to $expected")
  }

    test("'classify should work for empty 'points' and empty 'means'") {
      val points: GenSeq[Point] = IndexedSeq()
      val means: GenSeq[Point] = IndexedSeq()
      val expected = GenMap[Point, GenSeq[Point]]()
      checkClassify(points, means, expected)
    }

    test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
      val points: GenSeq[Point] = IndexedSeq()
      val mean = new Point(1, 1, 1)
      val means: GenSeq[Point] = IndexedSeq(mean)
      val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
      checkClassify(points, means, expected)
    }

    test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
      val p1 = new Point(1, 1, 0)
      val p2 = new Point(1, -1, 0)
      val p3 = new Point(-1, 1, 0)
      val p4 = new Point(-1, -1, 0)
      val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
      val mean = new Point(0, 0, 0)
      val means: GenSeq[Point] = IndexedSeq(mean)
      val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))
      checkClassify(points, means, expected)
    }

    test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
      val p1 = new Point(1, 1, 0)
      val p2 = new Point(1, -1, 0)
      val p3 = new Point(-1, 1, 0)
      val p4 = new Point(-1, -1, 0)
      val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
      val mean1 = new Point(1, 0, 0)
      val mean2 = new Point(-1, 0, 0)
      val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
      val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
      checkClassify(points, means, expected)
    }

    def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
      assert(classify(points.par, means.par) == expected,
        s"classify($points par, $means par) should equal to $expected")
    }

    test("'classify with data parallelism should work for empty 'points' and empty 'means'") {
      val points: GenSeq[Point] = IndexedSeq()
      val means: GenSeq[Point] = IndexedSeq()
      val expected = GenMap[Point,GenSeq[Point]]()
      checkParClassify(points, means, expected)
    }

  /*
[Test Description] 'update' should work for 'classified' points with one Point as key
  and same Point as value and 'means' with the same Point

[Observed Error] Util.equalPointSeq(KM.update(classified, oldMeans), expected) was false
update(Map((0.0, 0.0, 0.0) -> List((0.0, 0.0, 0.0))), Vector((0.0, 0.0, 0.0))) should equal to Vector((0.0, 0.0, 0.0))
[Lost Points] 1

def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point]
  */
  test("update test 0.0") {
    val point:Point = new Point(0.0, 0.0, 0.0)
    val oldMeans = GenSeq(point)
    val points = GenSeq(point)
    val means: GenSeq[Point] = update(GenMap(point -> points), oldMeans)
    val expected = GenSeq(point)
    assert(means.mkString == expected.mkString, "they are not ok")
  }
/*
    [Test Description] 'converged' should work for empty oldMeans and empty newMeans
    [Observed Error] false did not equal true converged(Vector(), Vector()) should equal to true
    [Lost Points] 2
*/
  test("converged should work for empty oldMeans and empty newMeans") {
    assert(converged(1)(GenSeq(),GenSeq()) == true)
  }

  /*
[Test Description] 'kMeans' should work for 'points' == GenSeq((0, 0, 1), (0,0, -1), (0,1,0), (0,10,0)) and
'oldMeans' == GenSeq((0, -1, 0), (0, 2, 0)) and 'eta' == 12.25

[Observed Error] test has been aborted
[Lost Points] 4
  */
  test("'kMeans' should work for .....") {
    val points = GenSeq(new Point(0, 0, 1), new Point(0,0, -1), new Point(0,1,0), new Point(0,10,0))
    val oldMeans = GenSeq(new Point(0, -1, 0), new Point(0, 2, 0))
    val eta = 12.25
    //final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point]
    assert( kMeans(points, oldMeans, eta).mkString == GenSeq(new Point(0.0, 0.0, 0.0), new Point(0.0, 5.5, 0.0)).mkString)
  }

  /*
[Test Description] 'update' should work for 'classified' points with one Point as key
  and empty value for the key and 'means' with the same Point
[Observed Error] Util.equalPointSeq(KM.update(classified, oldMeans), expected) was false
update(Map((0.0, 0.0, 0.0) -> List()), Vector((0.0, 0.0, 0.0))) should equal to Vector((0.0, 0.0, 0.0))
[Lost Points] 1

def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point]
  */
  test("update' should work for 'classified' points with one Point as key and empty value for the key and 'means' with the same Point") {
    val point = new Point(0.0, 0.0, 0.0)
    val classified = Map(point -> List())
    val oldMeans = GenSeq(point)
    println("here:")
    println("value:"+classified(point))
    assert( update(classified, oldMeans).mkString == GenSeq(new Point(0.0, 0.0, 0.0)).mkString)
  }
}


  
