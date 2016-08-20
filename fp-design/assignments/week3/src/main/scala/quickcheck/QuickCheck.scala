package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (h: Int) =>
    val m = insert(h, empty)
    findMin(m) == h
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("isEmpty1") = forAll { (h: H) =>
    isEmpty(empty)
  }

  property("findMin") = forAll { (x: Int, y:Int) =>
    val queue = insert(x, insert(y, empty))
    val target = findMin(queue)
    target == math.min(x, y)
  }

  property("empty->insertList->delete") = forAll{ (xs: List[Int]) =>
    val queue = xs.foldRight(empty)((x:Int, agg) => insert(x, agg))
    val target = expand(queue, Nil)
    isEmpty(target._1) && target._2.length == xs.length && compare(target._2)
  }

  property("arbitrary element and heap") = forAll { (a: Int, h: H) =>
    findMin(insert(a, h)) == math.min(a, findMin(h))
  }

  property("isSequenced") = forAll { (h: H) =>
    val target = expand(h, Nil)
    val expanded = target._2
    //println(expanded)
    isEmpty(target._1) && compare(expanded)
  }

  property("meld1") = forAll { (x: H, y: H) =>
    val xmin = findMin(x)
    val ymin = findMin(y)

    val xy = meld(x, y)
    val target = findMin(xy)
    if (xmin < ymin) xmin == target
    else ymin == target
  }

  property("deleteMin1") = forAll { (x: Int, y: Int) =>
    val xs = insert(x, empty)
    val ys = insert(y, xs)
    val preMin = findMin(ys)
    val dys = deleteMin(ys)
    val postMin = findMin(dys)

    if(x < y) {
      preMin == x && postMin == y && !isEmpty(dys)
    }
    else {
      preMin == y && postMin == x && !isEmpty(dys)
    }
  }

  property("delete(h1)->insert(h1.min, h2)->meld == meld(h1, h2)") = forAll { (h1: H, h2: H) =>

    val d1 = deleteMin(h1)
    val i1 = insert(findMin(h1), h2)

    val oddmelded = meld(d1, i1)
    val melded = meld(h1, h2)

    val oddExpand = expand(oddmelded, Nil)
    val expanded = expand(melded, Nil)

    oddExpand == expanded
  }

  def compare(xs: List[Int]): Boolean = xs match {
    case Nil => true
    case x :: tail if tail exists (y => x > y) => false
    case x :: tail if tail forall(y => x <= y) => compare(tail)
  }

  def expand(h: H, accumulator: List[Int]) : (H, List[Int]) = {
    def next(elem: H, acc: List[Int]): (H, List[Int]) = {
      val min = findMin(elem)
      val del = deleteMin(elem)
      (del, min :: acc)
    }

    if (isEmpty(h)) {
      (h, accumulator reverse)
    }
    else {
      val t = next(h, accumulator)
      expand(t._1, t._2)
    }
  }

}
