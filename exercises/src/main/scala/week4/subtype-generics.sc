import scala.collection.immutable.Stream.Empty

val a: Array[NonEmpty] = Array(new NonEmpty)
// type error here with
//  Expression of type Array[NonEmpty] doesn't confirm to expected type Array[IntSet]
val b: Array[IntSet] = a

//b(0) = Empty

val s: NonEmpty = a(0)

class IntSet {}
class NonEmpty extends IntSet{}
class Empty extends IntSet{}