package week4

class FRPBankAccount {
  val balance = Var(0)

  def deposit(amount: Int): Unit = {
    if(amount > 0) {
      val b = balance()
      balance() = b + amount
    }
  }

  def withdraw(amount: Int): Int =
    if(0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
      balance()
    } else throw new Error("insufficient funds")
}
