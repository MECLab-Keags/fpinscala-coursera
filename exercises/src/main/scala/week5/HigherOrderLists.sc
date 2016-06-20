
def squareList(xs: List[Int]) : List[Int] = xs match {
    case Nil => xs
    case h :: t => h * h :: squareList(t)
}

def squareList2(xs: List[Int]) : List[Int] = xs map (x => x * x)

squareList(List(1,2,3,4,5))
squareList2(List(1,2,3,4,5))


def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case head :: tail =>
        val (first, rest) = xs span (x => x == head)
        first :: pack(rest)
}

val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)

def encode[T](xs: List[T]): List[(T,Int)] = pack(xs) map (ys => (ys.head, ys.length))
encode(data)