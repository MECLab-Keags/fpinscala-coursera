import week4.{BankAccount, Consolidator}

val account = new BankAccount

account deposit 50
account withdraw 20
account withdraw 20


val a = new BankAccount
val b = new BankAccount
val consolidator = new Consolidator(List(a, b))
consolidator.totalBalance

a deposit 100
consolidator.totalBalance

a withdraw 10
b deposit 100
consolidator.totalBalance

