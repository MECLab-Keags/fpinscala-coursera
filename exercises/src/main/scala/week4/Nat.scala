package week4

trait Nat{
    def isZero: Boolean

    def predecessor:Nat

    def successor: Nat = new Succ(this)

    def + (that: Nat): Nat
    def - (that: Nat): Nat
}
case object Zero extends Nat {
    def isZero: Boolean = true
    def predecessor: Nat = throw new Error("0.predecessor")
    def + (that: Nat): Nat = that
    def - (that:Nat): Nat = if(that.isZero) this else throw new Error("0.-")
}
case class Succ(val n: Nat) extends Nat {
    def isZero: Boolean = false
    def predecessor : Nat = n
    def +(that:Nat) = new Succ(n + that)
    def -(that:Nat) : Nat = if(that.isZero) this else n - that.predecessor
}
