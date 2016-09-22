package week3

import org.scalameter._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.parallel.Combiner
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.forkjoin.ForkJoinPool
import scala.reflect.ClassTag

class ArrayCombiner[T <: AnyRef: ClassTag](val parallelism: Int) extends Combiner[T, Array[T]] {
  private var numElements = 0
  private val buffers = new ArrayBuffer[ArrayBuffer[T]]

  def += (x: T) = {
    try {
      if (buffers.isEmpty) buffers += ArrayBuffer(x)
      else buffers.last += x

      numElements += 1
      this
    } catch {
      case ex: Exception => {
        println("+=", ex)
        throw ex
      }
    }
  }

  def combine[N <: T, That >: Array[T]](that: Combiner[N, That]) = {
    (that: @unchecked) match {
      case that: ArrayCombiner[T] =>
        buffers ++= that.buffers
        numElements += that.numElements
        this
    }
  }
  def size = numElements
  def clear() = buffers.clear()

  def result: Array[T] = {
    val array = new Array[T](numElements)
    val step = math.max(1, numElements / parallelism)
    val starts = (0 until numElements by step) :+ numElements
    val chunks = starts.zip(starts.tail)
    val tasks: Seq[Future[Unit]] = for((from,end) <- chunks) yield Future {
      copyTo(array, from, end)
    }
    val future = Future.sequence(tasks)
    Await.result(future, 30.seconds)
    array
  }

  private def copyTo(array: Array[T], from: Int, end: Int): Unit = {
    var i = from
    var j = 0
    while (i >= buffers(j).length) {
      i -= buffers(j).length
      j += 1
    }

    var k = from
    while (k < end) {
      array(k) = buffers(j)(i)
      i += 1
      if(i >= buffers(j).length) {
        i = 0
        j += 1
      }
      k += 1
    }
  }
}

object ArrayCobinerRunner {
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) = {
    val size = 100000
    def run(p: Int): Unit = {
      val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(p))
      val strings = (0 until size) map(_.toString)
      val time = standardConfig measure {
        val parallelized = strings.par
        parallelized.tasksupport = taskSupport

        def newCombiner = new ArrayCombiner(p): Combiner[String, Array[String]]
        parallelized.aggregate(newCombiner)(_ += _, _ combine _).result
      }
      println(s"p = $p, time = $time ms")
    }

    run(1)
    run(2)
    run(4)
    run(8)
  }
}
