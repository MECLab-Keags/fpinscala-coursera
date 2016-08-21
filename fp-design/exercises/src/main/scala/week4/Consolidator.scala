package week4

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed foreach(_.subscribe(this))

  private var total: Int = compute()

  private def compute() : Int = {
    total = observed.map(_.currentBalance).sum
    total
  }

  def handler(publisher: Publisher) = compute()

  def totalBalance = total
}
