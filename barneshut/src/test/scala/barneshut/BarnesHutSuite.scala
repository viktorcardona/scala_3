package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

import FloatOps._
  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    println("body.xspeed:"+body.xspeed)
    println("body.yspeed:"+body.yspeed)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
                   //mass, x , y , xspeed ,yspeed
    val body = new Body(5, 25, 47, 0.1f   , 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.+=' should add a body at (0,0) to the correct bucket of a sector matrix of size 96") {
    //mass, x , y , xspeed ,yspeed
    val body = new Body(5, 1, 1, 0.1f   , 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(0, 0).size == 1 && sm(0, 0).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.+=' should add a body(0, 0) at (0,0) to the correct bucket of a sector matrix of size 96") {
    //mass, x , y , xspeed ,yspeed
    val body = new Body(5, 0, 0, 0.1f   , 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(0, 0).size == 1 && sm(0, 0).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.+=' should add a body(97, 97) at (7,7) to the correct bucket of a sector matrix of size 96") {
    //mass, x , y , xspeed ,yspeed
    val body = new Body(5, 97, 97, 0.1f   , 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(7, 7).size == 1 && sm(7, 7).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  /*[Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100
[Observed Error] res was false Body not found in the right sector. Hint: sector sizes could be fractions
[Lost Points] 2*/

  /*
  test("'SectorMatrix.+=' should add a body() at (25, 47) to the correct bucket of a sector matrix of size 100 !!!") {
                    //mass, x , y , xspeed ,yspeed
    val body = new Body(5, 25, 47, 0.1f   , 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }


  test("'SectorMatrix.combine' should works...") {
    //mass, x , y , xspeed ,yspeed
    val body0 = new Body(5, 27, 2, 0.1f   , 0.1f)
    val body = new Body(5, 25, 47, 0.1f   , 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 101
    boundaries.maxY = 101
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)

    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body1 not found in the right sector 1")

    sm += body0
    val res0 = sm(2, 0).size == 1 && sm(2, 0).find(_ == body0).isDefined
    assert(res0, s"Body0 not found in the right sector 1")

    val body2 = new Body(5, 28, 38, 0.1f   , 0.1f)
    val body3 = new Body(5, 75, 15, 0.1f   , 0.1f)
    val boundaries2 = new Boundaries()
    boundaries2.minX = 1
    boundaries2.minY = 1
    boundaries2.maxX = 101
    boundaries2.maxY = 101

    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm2 += body2
    val res2 = sm2(2, 3).size == 1 && sm2(2, 3).find(_ == body2).isDefined
    assert(res2, s"Body2 not found in the right sector 2")
    sm2 += body3
    val res21 = sm2(6, 1).size == 1 && sm2(6, 1).find(_ == body3).isDefined
    assert(res21, s"Body3 not found in the right sector 2")

    val sm3 = sm.combine(sm2)
    val res3 = sm3(2, 3).size == 2 && sm3(2, 3).find(_ == body).isDefined &&
               sm3(2, 3).find(_ == body2).isDefined &&
               sm3(6, 1).size == 1 && sm3(6, 1).find(_ == body3).isDefined &&
               sm3(2, 0).size == 1 && sm3(2, 0).find(_ == body0).isDefined

    assert(res3, s"Body1, body2 and body3 not found in the right sector!!!!")
  }
*/

  /*[Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)
[Observed Error] res was false Body 3 not found in the right sector in combined sector matrix
[Lost Points] 2*/
  test("'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)...") {
    //mass, x , y , xspeed ,yspeed
    val body1 = new Body(5, 12, 34, 0.1f, 0.1f)
    val body2 = new Body(5, 23, 45, 0.1f, 0.1f)
    val body3 = new Body(5, 56, 9, 0.1f, 0.1f)
    val body4 = new Body(5, 8, 79, 0.1f, 0.1f)
    val body5 = new Body(5, 5, 99, 0.1f, 0.1f)

    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 96
    boundaries.maxY = 96
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)

    sm += body1
    /*
    sm += body2
    sm += body3
    sm += body4
    sm += body5
    */

    /*
    https://www.coursera.org/learn/parprog1/discussions/weeks/4/threads/VYBizSY4EeacKhJ2Eiev7Q
    each sector k has the interval:
    [1 + k * 12, 1 + (k + 1) * 12>
    This convention usually simplifies the calculation of the sector that the point belongs to.

    So, everything in the interval [1+0, 1+96/8=12> ends up in the sector 0,
    everything in [1 + 96/8=12, 1 + 2*96/8=24> in sector 1,
    and so on.
    So, the point (25, 47) is in the sector (2, 3).

    */

    val res = sm(0, 2).size == 1 && sm(0, 2).find(_ == body1).isDefined
    assert(res, s"Body1 not found in the right sector 1")
  }

  test("'SectorMatrix.+=' should add a body at (64,27) to the correct bucket of a sector matrix of size 108 when the sector precision is 12") {
    //mass, x , y , xspeed ,yspeed
    val body = new Body(5, 64, 27, 0.1f   , 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 0
    boundaries.minY = 0
    boundaries.maxX = 108
    boundaries.maxY = 108
    //13.5 = sectorSize
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(4, 1).size == 1 && sm(4, 1).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

