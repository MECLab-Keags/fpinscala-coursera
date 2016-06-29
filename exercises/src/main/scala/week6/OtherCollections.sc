
val xs = Array(1,2,3,44)
xs map (x => x * 2)

val s = "Hello World"
s filter (c => c.isUpper)
s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1,2,3,4) zip s
pairs.unzip

s flatMap (c => List('.',c))

xs.sum
xs.max

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(t => t._1 * t._2).sum

// Variation of scalarProduct using pattern matching
def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{ case (x,y) => x * y}.sum

scalarProduct(Vector(1,2,3), Vector(3,2,1))
scalarProduct2(Vector(3,4,5), Vector(3,2,1))


def isPrime(n:Int): Boolean = (2 until n) forall(x => n % x != 0)
isPrime(5)
isPrime(6)

