import calculator.{Signal, Var}
import frp.BankAccount

def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)

val a = new BankAccount()
val b = new BankAccount()
val c = consolidated(List(a,b))
c()

a deposit 20
c()
b deposit 30
c()

val xchange = Signal(246.00)
val inDollar = Signal(c() * xchange())
inDollar()
b withdraw 10
inDollar()

val num = Var(1)
val twice = Signal(num() * 2)
num() = 2
twice()

var num2 = Var(1)
val twice2 = Signal(num2() * 2)
num2 = Var(2)
twice2()