def factorial(n:Int):Int = {
  @annotation.tailrec
  def loop(acc:Int)(n:Int) : Int = n match {
    case n if n <= 0 => acc
    case _ => loop(acc * n)(n-1)
  }
  loop(1)(n)
}
factorial(-1)
factorial(0)
factorial(1*0)

/**
  * Exercise 1
  */
def pascal(c: Int, r: Int): Int =
  // the formula to calculate a given position within a pascal triangle is known as 'n choose k'
  // (taken from http://www.mathsisfun.com/pascals-triangle.html)
  // i.e. n!/k!(n-k)!
  // where ! is factorial
  // and n = row, k = column
  factorial(r) / (factorial(c) * factorial(r - c))

pascal(0,0)
pascal(-1,0)
pascal(0,-1)
pascal(-1,-1)

/**
  * Exercise 2
  */
def balance(chars: List[Char]): Boolean = {
  def loop(acc:Int)(cs:List[Char]) : Boolean = cs match {
    case h :: t => h match {
      case '(' => loop(acc + 1)(t)
      case ')' if acc == 0 => false
      case ')' => loop(acc - 1)(t)
      case _ => loop(acc)(t)
    }
    case Nil => acc == 0
  }
  loop(0)(chars)
}

/**
  * Exercise 3
  */
def countChange(money: Int, coins: List[Int]): Int = coins match {
  case h :: t => money match {
    case 0 => 1
    case m if m < 0 => 0
    case _ => countChange(money,t) + countChange(money - h,coins)
  }
  case Nil => 0
}