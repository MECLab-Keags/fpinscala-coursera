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
    def loop(acc:Int)(cs:Array[Char]) : Boolean = {
      if(cs.isEmpty) acc == 0
      else if(acc < 0) false
      else if(cs.head == '(') loop(acc + 1)(cs.tail)
      else if(cs.head == ')') loop(acc - 1)(cs.tail)
      else loop(acc)(cs.tail)
    }
    loop(0)(chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if(idx == until) (arg1, arg2)
      else {
        val char = chars(idx)
        if(char == '(') {
          if(arg1 < 0) {
            //println("open if", "char: '" + char + "'", "idx: " + idx, "until:" + until, arg1, arg2)
            traverse(idx+1, until, arg1, arg2 + 1)
          }
          else {
            //println("open else", "char: '" + char + "'", "idx" + idx, "until:" + until, arg1, arg2)
            traverse(idx + 1, until, arg1 + 1, arg2)
          }
        }
        else if(char == ')') {
          if(arg1 < 0) {
            //println("close if", "char: '" + char + "'", "idx" + idx, "until:" + until, arg1, arg2)
            traverse(idx+1, until, arg1, arg2 - 1)
          }
          else {
            //println("close else", "char: '" + char + "'", "idx" + idx, "until:" + until, arg1, arg2)
            traverse(idx+1, until, arg1 - 1, arg2)
          }
        }
        else traverse(idx+1, until, arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((x1,y1), (x2,y2)) = parallel(reduce(from, mid), reduce(mid, until))
        if(x1 < 0 && x2 > 0) (x1, x2 + y1 + y2)
        else if(x2 < 0 && y1 > 0) (x1 + x2 + y1, y2)
        else (x1 + x2, y1 + y2)
      }
    }

    val (left, right) = reduce(0, chars.length)
    left + right == 0 && left >= 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
