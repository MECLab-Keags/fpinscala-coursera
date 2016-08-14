trait RNG {
    def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}
object RNG {
    def nonNegativeInt(rng: RNG) = {
        def go(curr: RNG) : (Int, RNG) = {
            val (n, next) = curr.nextInt
            if (n >= 0) return (n, next)
            go(next)
        }
        go(rng)
    }
    // Answer from fpinscala
    def nonNegativeIntAns(rng: RNG) : (Int, RNG) = {
        val (n, next) = rng.nextInt
        (if (n < 0) -(n + 1) else n, next)
    }

    def double(rng: RNG) : (Double, RNG) = {
        val (n, next) = RNG.nonNegativeIntAns(rng)
        (n / (Int.MaxValue.toDouble + 1), next)
    }
}

val rng42 = SimpleRNG(42)
val (n421, rng421) = rng42.nextInt
val (n422, rng422) = rng421.nextInt
val (n423, rng423) = rng422.nextInt
val (n424, rng424) = rng423.nextInt
val (n425, rng425) = rng424.nextInt

val rng0 = SimpleRNG(1)
val (n1, rng1) = rng0.nextInt
val (n2, rng2) = rng1.nextInt

Int.MinValue
Int.MaxValue

val rngNoNeg = SimpleRNG(42)
val (neg1, rngNeg1) = RNG.nonNegativeInt(rngNoNeg)
val (neg2, rngNeg2) = RNG.nonNegativeInt(rngNeg1)

val rngNoNeg2 = SimpleRNG(42)
val (neg21, rngNeg21) = RNG.nonNegativeIntAns(rngNoNeg2)
val (neg22, rngNeg22) = RNG.nonNegativeIntAns(rngNeg21)

Int.MaxValue.toDouble + 1
16159453 / Int.MaxValue.toDouble + 1
1281479696 / Int.MaxValue.toDouble + 1

val rngD0 = SimpleRNG(42)
val (d1, rngD1) = RNG.double(rngD0)
val (d2, rngD2) = RNG.double(rngD1)
val (d3, rngD3) = RNG.double(rngD2)
val (d4, rngD4) = RNG.double(rngD3)
val (d5, rngD5) = RNG.double(rngD4)

