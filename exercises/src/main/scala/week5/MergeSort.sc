import math.Ordering

def mergeSort[T](xs:List[T])(ord: Ordering[T]) : List[T] = {
    val n = xs.length / 2
    if(n == 0) xs
    else {
        def merge(xs: List[T], ys: List[T]) : List[T] = (xs,ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (h1 :: t1, h2 :: t2) => if(ord.lt(h1,h2)) h1 :: merge(t1, ys) else h2 :: merge(xs, t2)
        }
        val (fst,snd) = xs splitAt n
        merge(mergeSort(fst)(ord), mergeSort(snd)(ord))
    }
}

val nums = List(2,5,1,78,-4,10,48,5,2)
mergeSort(nums)(Ordering.Int)