package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal(b()*b() - 4 * a() * c())

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val minusB = -b()
    val sqrtDelta = Math.sqrt(delta())
    val doubleA = a() * 2
    val result = Set((minusB + sqrtDelta) / doubleA, (minusB - sqrtDelta) / doubleA)
    println("a: " + a(), "b: " + b(), "c: " + c(), "result: " + result)
    result
  }
}
