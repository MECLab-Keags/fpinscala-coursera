import week4._

val account = new ObservableBankAccount

account deposit 50
account withdraw 20
account withdraw 20


val a = new ObservableBankAccount
val b = new ObservableBankAccount
val consolidator = new Consolidator(List(a, b))
consolidator.totalBalance

a deposit 100
consolidator.totalBalance

a withdraw 10
b deposit 100
consolidator.totalBalance

def consolidate(accts: List[FRPBankAccount]): Signal[Int] =
  Signal(accts map(_.balance()) sum)

val frpA = new FRPBankAccount
val frpB = new FRPBankAccount

val c = consolidate(List(frpA, frpB))

frpA deposit 100
frpA balance()
c()

frpB deposit 50
c()

val xe = Signal(246.00)
val inDollar = Signal(c() * xe())
inDollar()
frpB withdraw 10
inDollar()


val num = Var(1)
val twice = Signal(num() * 2)
twice()
num() = 2
twice()

var num2 = Var(1)
val twice2 = Signal(num2() * 2)
twice2()
num2 = Var(2)
twice2()

