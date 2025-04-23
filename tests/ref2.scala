struct Ref(var x: i32)
def par[cap C1, cap C2](op1: () ->{C1} Unit, op2: () ->{C2} Unit): Unit = ()
def show(x: Ref^): Unit =
  #i32println(x.x)
def test(a: Ref^, b: Ref^): Unit =
  par[{a}, {b}](() => show(a), () => show(b))
  par[{a}, {a}](() => show(a), () => show(a))
  ()
