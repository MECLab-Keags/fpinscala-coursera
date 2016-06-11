package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet
  
  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = fold[TweetSet](List(this), acc)((ts, tss) => ts.tweet :: ts.leftTweetSet :: ts.rightTweetSet :: tss)((ts, t) => if(p(t)) ts.incl(t) else ts)

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = fold[TweetSet](List(this, that), new Empty)((ts, tss) => ts.tweet :: ts.leftTweetSet :: ts.rightTweetSet :: tss)((ts, t) => ts.incl(t))
  
  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet = ???
  
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = ???
  
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def foldRight[A](acc: => A)(f: (Tweet, A) => A) : A = acc

  def foldLeft[A](z: => A)(op: (A, Tweet) => A) : A = z

  def fold[A](as: List[Any], acc: => A)(f: (NonEmpty, List[Any]) => List[Any])(op: (A, Tweet) => A) : A = as match {
    case (head:NonEmpty) :: tail  => fold(f(head, tail), acc)(f)(op)
    case (head:Tweet) :: tail => fold(tail, op(acc, head))(f)(op)
    case (head:Empty) :: tail => fold(tail, acc)(f)(op)
    case _ => acc
  }
}

class Empty extends TweetSet {

  override def filter(p: Tweet => Boolean): TweetSet = this

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  val tweet: Tweet = elem
  val leftTweetSet = left
  val rightTweetSet = right

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def foldRight[A](acc: => A)(f: (Tweet, A) => A) : A = f(elem, (left union right).foldRight(acc)(f))

  override def foldLeft[A](acc: => A)(op: (A, Tweet) => A) : A = {
    val acc1 = op(acc, elem)
    val acc2 = left.foldLeft(acc1)(op)
    val acc3 = right.foldLeft(acc2)(op)
    acc3
  }

  /**
    * This method takes a predicate and returns a subset of all the elements
    * in the original set for which the predicate is true.
    *
    * Question: Can we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
  */
  override def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  def filter3(p: Tweet => Boolean): TweetSet = fold[TweetSet](List(this), new Empty)((ts:NonEmpty, tss:List[Any]) => ts.tweet :: ts.leftTweetSet :: ts.rightTweetSet :: tss)((acc, t) => t match {
    case t if(p(t)) => acc.incl(t)
    case _ => acc
  })

  def filter2(p: Tweet => Boolean): TweetSet = foldLeft[TweetSet](new Empty)((ts, t) => t match {
    case t if(p(t)) => ts.incl(t)
    case _ => ts
  })

  def filter1(p: Tweet => Boolean): TweetSet = foldRight((new Empty) : TweetSet)((t,acc) => t match {
    case t if(p(t)) => acc.incl(t).union(left.filter(p)).union(right.filter(p))
    case _ => left.filter(p).union(right.filter(p))
  })

  def union1(that: TweetSet) : TweetSet = ((left union right) union that) incl elem

  override def mostRetweeted: Tweet = fold(List(this), this.tweet)((ts, tss) => ts.tweet :: ts.leftTweetSet :: ts.rightTweetSet :: tss)((curr, t) => if(t.retweets > curr.retweets) t else curr)

  def mostRetweeted2: Tweet = foldLeft(elem)((that, t) => t match {
    case x if(x.retweets > that.retweets) => x
    case _ => that
  })

  def mostRetweeted1: Tweet = foldRight(this.elem)((t, that) => t match {
    case x if(x.retweets > that.retweets) => x
    case _ => that
  })

  override def descendingByRetweet: TweetList = fold[TweetList](List(this), Nil)((ts, tss) => ts.tweet :: ts.leftTweetSet :: ts.rightTweetSet :: tss)((acc,t) => acc match {
    case Nil => new Cons(t, acc)
    case Cons(head, tail) if(head.retweets > t.retweets) => {
      val x = new Cons(t, tail.filter(c => c.retweets < t.retweets)) append new Cons(head, tail.filter(c => c.retweets > t.retweets))
      print("head gt curr = head:" + head.retweets + " curr:" + t.retweets + " tail:");println(x.foreach(c => print(c.retweets)))
      x
    }
    case Cons(head, tail) => {
      val x = new Cons(t, new Cons(head, tail.filter(c => c.retweets < t.retweets))) append tail.filter(c => c.retweets > t.retweets)
      print("head gt curr = head:" + head.retweets + " curr:" + t.retweets + " tail:");println(x.foreach(c => print(c.retweets)))
      x
    }
  })

  def descendingByRetweet3: TweetList = fold[TweetList](List(this), Nil)((ts, tss) => ts.tweet :: ts.leftTweetSet :: ts.rightTweetSet :: tss)((acc,t) => acc match {
    case Nil => new Cons(t, Nil)
    case Cons(head, tail) if(head.retweets > t.retweets) => new Cons(head, new Cons(t,tail))
    case Cons(head, tail) => new Cons(t, acc)
  })

  def descendingByRetweet2: TweetList = foldLeft[TweetList](Nil)((acc, t) => acc match {
    case Nil => new Cons(t, Nil)
    case Cons(head, tail) if(head.retweets < t.retweets) => new Cons(t, acc)
    case Cons(head, tail) => new Cons(head, new Cons(t, tail))
  })

  def descendingByRetweet1: TweetList = foldRight[TweetList](objsets.Nil)((t,acc) => acc match {
    case Nil => new Cons(t, Nil)
    case Cons(head, tail) if(head.retweets < this.elem.retweets) => new Cons(this.elem, new Cons(head, tail))
    case tl => new Cons(tl.head, new Cons(this.elem, tl.tail))
  })
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  @annotation.tailrec
  final def foldLeft(tl: TweetList)(acc: TweetList)(f: (TweetList, Tweet) => TweetList) : TweetList = tl match {
    case Cons(head, tail) => foldLeft(tail)(f(acc, head))(f)
    case _ => acc
  }

  def filter(p: Tweet => Boolean) = foldLeft(this)(Nil)((acc, t) => if(p(t)) new Cons(t, acc) else acc)

  def append(rest: TweetList): TweetList = foldLeft(this)(rest)((acc, t) => new Cons(t, acc))
  //def union(rest: => TweetList) : TweetList = if(isEmpty) rest else Cons(head, tail union rest)
}
case object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}
case class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def search(ts: TweetSet)(keyword: List[String]) : TweetSet = ts.filter(t => t.text.split(' ').exists(word => keyword.exists(key => key == word)))
  lazy val googleTweets: TweetSet = search(TweetReader.allTweets)(google)
  lazy val appleTweets: TweetSet = search(TweetReader.allTweets)(apple)
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets union appleTweets) descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
