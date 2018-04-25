package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected with input [ $input ]")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }


  test("Parallel ---------------------> balance should work for string of differents lengths.") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,//13
        s"balance($input) should be $expected with input [ $input ]")

        check("()", true)
        check(")(", false)

        check("((", false)
        check("))", false)
        check(".)", false)
        check(".(", false)
        check("(.", false)
        check(").", false)
        check("(if (zero? x) max (/ 1 x))", true)


    check("I told him (that it's not (yet) done). (But he wasn't listening)", true)
    check("..(..('.)..)..(..)", true)
    check("I told him (that it's not (yet) done)..(But he wasn't listening)", true)

    check("(o_()", false)
    check(":-)", false)


    check("())(", false)

    /*
    [Test Description] parBalance should work for nested parentheses and threshold 1
[Observed Error] false did not equal true parbalance('(())') should be true
[Lost Points] 5
    */

    check("(())", true)
  }


}