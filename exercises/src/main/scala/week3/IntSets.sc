
abstract class IntSet {
    def include(x:Int): IntSet
    def contains(x:Int): Boolean
    def union(other:IntSet) : IntSet
}

class Empty extends IntSet {
    override def include(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

    override def contains(x: Int): Boolean = false

    override def toString = "."

    override def union(other: IntSet): IntSet = other
}

class NonEmpty (elem:Int, left:IntSet, right:IntSet) extends IntSet {

    override def include(x: Int): IntSet =
        if(x < elem) new NonEmpty(elem, left include x, right)
        else if(x > elem) new NonEmpty(elem, left, right include x)
        else this


    override def contains(x: Int): Boolean =
        if(x < elem) left contains x
        else if(x > elem) right contains x
        else true

    override def toString = "{" + left + elem + right + "}"

    override def union(other: IntSet): IntSet = other
}

val s1 = new NonEmpty(3, new Empty, new Empty)
val s2 = s1 include 4
val s3 = s2 include 10
val s4 = s2 include 1



