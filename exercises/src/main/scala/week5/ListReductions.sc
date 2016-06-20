def concat[T](xs: List[T], ys: List[T]) : List[T] =
    (xs foldRight ys)(_ :: _)

val data = List("a", "a", "a", "b", "c", "c", "a")
concat(data, data)