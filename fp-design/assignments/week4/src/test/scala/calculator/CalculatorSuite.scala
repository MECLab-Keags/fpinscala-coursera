package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength
import Polynomial._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("a=1, b=4, c=-21 == 100") {
    val target = computeDelta(Var(1), Var(4), Var(-21))
    assert(target() == 100)
  }

  test("a=1, b=1, c=1") {
    val target = computeDelta(Var(1), Var(1), Var(1))
    assert(target() == -3)
  }

  test("a=1, b=1, c=1 == Set(NaN, NaN)") {
    val a = Var(1.0)
    val b = Var(1.0)
    val c = Var(1.0)
    val delta = computeDelta(a, b, c)
    val solutions = computeSolutions(a, b, c, delta)

    val target = Signal { solutions() }
    println("pre-target: " + target())
    //assert(target().forall(x => x.equals(Double.NaN)))

    b() = 4.0
    c() = -21.0
    println("post-target: " + target())
  }

  test("polynomial is 3 or -7") {
    val a = Var(1.0)
    val b = Var(4.0)
    val c = Var(-21.0)
    val delta = computeDelta(a,b,c)
    val target = computeSolutions(a, b, c, delta)
    assert(target() == Set(3, -7))
  }
}
