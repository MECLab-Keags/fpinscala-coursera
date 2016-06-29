def isPrime(n:Int): Boolean = (2 until n) forall(x => n % x != 0)


val n = 7
val xss = (1 until n) map (i => (1 until i) map (j => (i,j)))

(xss foldRight Seq[(Int,Int)]())(_ ++ _)
xss.flatten

(1 until n) flatMap (i =>
    (1 until i) map (j => (i,j))) filter (p => isPrime(p._1 + p._2))

for {
    i <- 1 until n
    j <- 1 until i
    if(isPrime(i + j))
} yield (i,j)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (for ((x,y) <- xs zip ys) yield x * y).sum

scalarProduct(Vector(1,2,3), Vector(3,2,1))