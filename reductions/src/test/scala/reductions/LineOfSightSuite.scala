package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  /*
  [Test Description] downsweep should correctly handle trees with a single leaf
[Observed Error] List(0.0, 12.0, 12.0, 12.0, 12.0) did not equal List(0.0, 7.0, 7.0, 11.0, 12.0)
[Lost Points] 1
  */

  test("downsweep should correctly handle trees with a single leaf v0") {
    val output = new Array[Float](4)
    downsweep(Array[Float](0f, 1f, 8f, 9f), output, 0f, Leaf(0, 4, 0))
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep should correctly handle trees with a single leaf v1") {

    val output = new Array[Float](5)
    downsweep(Array[Float](0f, 7f, 7f, 33f, 48f), output, 0f, Leaf(0, 5, 12))
    assert(output.toList == List(0.0, 7.0, 7.0, 11.0, 12.0))

    /*
    val output = new Array[Float](5)
    lineOfSight(Array[Float](0f, 7f, 7f, 33f, 48f), output)
    assert(output.toList == List(0.0, 7.0, 7.0, 11.0, 12.0))
    */
  }

  test("parLineOfSight threshold=1 should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 1)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight threshold=4 should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight threshold=2 should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 2)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight threshold=3 should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 3)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep with a single Leaf") {
    val output = new Array[Float](5)
    downsweep(Array[Float](0, 7, 6, 33, 48), output, 0f, Leaf(1, 5, 12f))
    assert(output.toList == List[Float](0, 7, 7, 11, 12))
  }

}

