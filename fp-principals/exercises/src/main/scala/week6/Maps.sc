
class Poly(val initial: Map[Int,Double]) {
    val terms = initial withDefaultValue 0.0

    def this(bindings: (Int,Double)*) = this(bindings.toMap)

    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def plus (other: Poly) = new Poly(terms ++ (other.terms map adjust2))
    def plus2 (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int,Double], term:(Int,Double)) : Map[Int,Double] = {
        val (exp, coeff) = term
        terms + (exp -> (coeff + terms(exp)))
    }


    def adjust(term: (Int,Double)): (Int,Double) = {
        val (exp,coeff) = term
        terms get exp match {
            case Some(c) => exp -> (coeff + c)
            case None => exp -> coeff
        }
    }

    def adjust2(term: (Int,Double)): (Int,Double) = {
        val (exp,coeff) = term
        exp -> (coeff + terms(exp))
    }

    override def toString = (for((exp,coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2
p1 plus p2
p1 plus2 p2

p1.terms(7)