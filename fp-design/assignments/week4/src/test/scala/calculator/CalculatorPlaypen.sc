import calculator.Polynomial._
import calculator.{Signal, Var}

val a = Var(1.0)
val b = Var(1.0)
val c = Var(1.0)
val delta = Signal(computeDelta(a, b, c))
val solutions = computeSolutions(a, b, c, delta())

val target = Signal {
  println("signal...")
  solutions()
}
println("pre-target: " + target())
//assert(target().forall(x => x.equals(Double.NaN)))

b() = 4.0
c() = -21.0
println("b: " + b(), "c: " + c(), "post-target: " + target())

a() = 1
b() = 4
c() = 1

a() = -5.3
b() = 4
c() = 1