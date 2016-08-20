import quickcheck.IntHeap

case class Node(x: Int, r: Int, c: List[Node])

def compare(xs: List[Int]): Boolean = xs match {
  case Nil => true
  case x :: tail if tail exists (y => x > y) => false
  case x :: tail if tail forall(y => x <= y) => compare(tail)
}

val xs = List(-2147483648, -1, -1, 0, 2147483647)
compare(xs)

List(Node(-1,1,List(Node(1,0,List()))), Node(-4719648,2,List(Node(1,1,List(Node(2147483647,0,List()))), Node(2147483647,0,List()))))
List(Node(1,0,List()))
List(Node(-4719648,2,List(Node(1,1,List(Node(2147483647,0,List()))), Node(2147483647,0,List()))))