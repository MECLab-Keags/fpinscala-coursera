val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

x - y - z
y + y
x < y

class Rational(n:Int, d:Int){
  private def gcd(x:Int, y:Int) : Int = if(y == 0) x else gcd(y, x % y)

  def numerator = n
  def denominator = d

  def + (other:Rational) : Rational = new Rational(
    numerator * other.denominator + other.numerator * denominator,
    denominator * other.denominator)

  def unary_- : Rational = new Rational(-numerator, denominator)
  def - (that:Rational) : Rational = this + -that
  def < (that:Rational) : Boolean = numerator * that.denominator < that.numerator * denominator
  def max(that:Rational) : Rational = if(this < that) that else this

  override def toString : String = {
    val g = gcd(n,d)
    numerator / g + "/" + denominator / g
  }
}