
def factorial(x:Int):Int =
  if(x==0) 1 else x * factorial(x-1)

def factorial2(x:Int):Int = {
  def loop(acc:Int)(n:Int) : Int =
    if (n==0) acc
    else loop(n*acc)(n-1)
  loop(1)(x)
}


factorial(4)
factorial2(4)

def pascal(c: Int, r: Int): Int =
  // the formula is n choose k
  // i.e. n!/k!(n-k)!
  // where ! is factorial
  // and n = row, k = column
  factorial2(r) / (factorial2(c) * factorial2(r - c))

pascal(0,1)
pascal(0,2)
pascal(1,2)
pascal(1,3)
pascal(2,4)


def count(money:Int, coins:List[Int]):Int = {
  if (money == 0) 1
  else if(coins.isEmpty || money < 0) 0
  else count(money, coins.tail) + count(money - coins.head, coins)
}
count(15, List(25, 10, 5, 1))

def count2(money:Int)(coins:List[Int]):Int = coins match {
  case h :: t => money match {
    case 0 => 1
    case m if m < 0 => 0
    case _ => {
      println(s"money:$money, h:$h")
      val trec = count2(money)(t)
      val hrec = count2(money - h)(coins)
      print(s"trec:$trec, hrec:$hrec")
      trec + hrec
    }
  }
  case Nil => 0
}
count2(15) (List(25, 10, 5, 1))
/*
def countChange(money:Int,coins:List[Int]):Int = (money,coins) match {
  case (m,c) if m <= 0 => 0
  case (_,c) if c.isEmpty => 0
  case _ => {
    def loop(agg:Int)(target:Int)(cs:List[Int]):Int = cs match {
      case h :: t =>
    }
  }
}
*/