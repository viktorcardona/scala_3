package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    //val first = chars.head
      var idx = 0;
      var balance = 0;
      while (idx < chars.size) {
        if (chars(idx) == '(') balance+=1
        else if (chars(idx) == ')') balance-=1

        if (balance < 0) return false

        idx+=1
      }
      balance == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      var idx2 = idx;
      var left = arg1
      var right = arg2
      var balance = 0
      var parentheses = 0//matchet parentheses
      while (idx2 < until) {
        if (chars(idx2) == '(') {
          left+=1
          balance+=1
        } else if (chars(idx2) == ')') {
          right+=1
          if ((balance - 1) >= 0) {
            balance-=1
            parentheses+=1
          }
        }
        idx2+=1
      }

      (right - parentheses, left - parentheses)//unmachet in the form =>   (  ))))),(((  ) => (5,3)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + ((until - from)/2)
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
        if (l._2 == r._1) (l._1, r._2)
        else if (l._2 > r._1) (l._1, r._2 + (l._2 - r._1))
        else (l._1 + (r._1 - l._2), r._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
