package week4

trait List[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
}
case class Cons[T](val head: T, val tail:List[T]) extends List[T]{
    def isEmpty = false
}
case object Nil extends List[Nothing]{
    override def isEmpty: Boolean = true
    override def head: Nothing = throw new NoSuchElementException("Nil.head")
    override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
    def apply[T]() = Nil
    def apply[T](x1:T, x2:T) : List[T] = new Cons(x1, Cons(x2, Nil))
    def apply[T](x1:T, x2:T, x3:T) : List[T] = new Cons(x1, Cons(x2, Cons(x3, Nil)))
}