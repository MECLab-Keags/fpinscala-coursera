
import reductions.ParallelParenthesesBalancing._

val b1 = "(if (zero? x) max (/ 1 x))".toArray
val b2 = "I told him (that it's not (yet) done). (But he wasn't listening)".toCharArray
val b3 = "(o_()".toArray
val b4 = ":-)".toArray
val b5 = "())(".toArray

balance("(".toCharArray)
balance("(if)".toCharArray)
balance(b1)
balance(b2)
balance(b3)
balance(b4)
balance(b5)

parBalance("(".toCharArray, 10)
parBalance("(if)".toCharArray, 10)
parBalance(b1, 10)
parBalance(b2, 10)
parBalance(b3, 10)
parBalance(b4, 10)
parBalance(b5, 10)

def mid(from: Int, until: Int) = from + (until - from) / 2