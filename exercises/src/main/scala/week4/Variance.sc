import week4._

def f(xs: List[NonEmpty], x: Empty) = xs prepend x

class IntSet {}
class NonEmpty extends IntSet{}
class Empty extends IntSet{}

def show(e:Expr)

trait Expr {
    def show() : String = this match{
        case Number(n) => "Number=%s".format(n)
        case Sum(e1, e2) => "Sum=%s".format(e1.eval + e2.eval)
    }

    def eval : Int = this match {
        case Number(n) => n
        case Sum(e1, e2) => e1.eval + e2.eval
    }
}
case class Number(n:Int) extends Expr{

}
case class Sum(e1:Expr, e2:Expr) extends Expr{}

