struct Ref(var x: i32)
def par[cap C1, cap C2](op1: () ->{C1} Unit, op2: () ->{C2} Unit): Unit = sorry()
def main(): Unit =
  val a = Ref(0)
  val b = Ref(0)
  val incA = () =>
    a.x = #i32add(a.x, 1)
  val incB = () =>
    b.x = #i32add(b.x, 1)
  par[{incA}, {incB}](incA, incB)
  ()
