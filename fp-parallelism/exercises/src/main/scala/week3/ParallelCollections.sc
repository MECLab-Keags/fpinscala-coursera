import java.util.concurrent._

import scala.collection._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

def solutionWithMutation(): Unit = {
  // as a result of side effects, and without proper synchronization the result of the following
  // is not deterministic.
  def intersection(a: GenSet[Int], b: GenSet[Int]): Set[Int] = {
    val result = mutable.Set[Int]()
    for (x <- a) if (b contains x) result += x
    result
  }

  val seqres = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
  val parres = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
  println(s"Sequential result - ${seqres.size}")
  println(s"Parallel result - ${parres.size}")
}
solutionWithMutation()
solutionWithMutation()
solutionWithMutation()


def solutionWithConcurrentMutation(): Unit ={
  def intersection(a: GenSet[Int], b: GenSet[Int]): ConcurrentSkipListSet[Int] = {
    val result = new ConcurrentSkipListSet[Int]()
    for(x <- a) if(b contains x) result.add(x)
    result
  }

  val seqres = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
  val parres = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
  println(s"Sequential result - ${seqres.size}")
  println(s"Parallel result - ${parres.size}")
}
solutionWithConcurrentMutation()
solutionWithConcurrentMutation()
solutionWithConcurrentMutation()


trait Iterator[T] {
  def hasNext: Boolean
  def next(): T
  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    if(!hasNext) z
    else {
      val next = next()
      foldLeft(f(z, next))(f)
    }
  }

  def foldLeft2[S](z:S)(f: (S,T) => S): S = {
    var result = z
    while(hasNext) result = f(result, next())
    result
  }
}

// Splitter is a counterpart of iterator used for parallel programming
trait Splitter[T] extends Iterator[T] {
  def split: Seq[Splitter[T]]
  def remaining: Int
  def threshold: Int

  def fold(z: T)(f: (T, T) => T): T = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children: Seq[Future[T]] = for(child <- split) yield Future { child.fold(z)(f) }
      // note this should not await as this will block.
      // we should just return the Future[T] instead.
      val result = Await.result(Future.sequence(children), 15.seconds)
                        .foldLeft(z)(f)
      result
    }
  }
}

trait Builder[T, Repr] {
  def += (elem: T): Builder[T, Repr]
  def result: Repr
}
trait Combiner[T, Repr] extends Builder[T, Repr] {
  def combine(that: Combiner[T, Repr]): Combiner[T, Repr]
}

trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: Builder[T, Traversable[T]]
  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach(t => if(p(t)) b += t)
    b.result
  }
}