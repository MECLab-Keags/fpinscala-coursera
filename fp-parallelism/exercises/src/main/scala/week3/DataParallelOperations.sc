
def sum(xs: Array[Int]): Int =
  xs.par.fold(0)(_+_)

def max(xs: Array[Int]): Int =
  xs.par.fold(Int.MinValue)(math.max)

val xs = Array(1,2,4,2,5,10)
sum(xs)
max(xs)



def play(a: String, b: String): String = List(a,b).sorted match {
  case List("paper", "scissors") => "scissors"
  case List("paper", "rock")     => "paper"
  case List("rock", "scissors")  => "rock"
  case List(a,b) if a == b => a
  case List("", b) => b
}

var plays = Array("paper", "rock", "paper", "scissors")
plays.fold("")(play)

// Limitation of fold is that the accumulator must be the same type collection type.
//   def fold(z:A)(f: (A,A) => A)
//Array('E','P','F','L').par.fold(0)((count, c) => if(true) count+1 else count)

def isVowel(char: Char): Boolean = {
  val vowels = List('A','E','I','O','U')
  vowels.exists(c => c == char.toUpper)
}
Array('E','P','F','L').par.aggregate(0)(
  (count, char) => if(isVowel(char)) count+1 else count,
  (x,y) => x + y)