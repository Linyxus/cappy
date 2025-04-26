struct Ref(var data: i32)
//struct RefPair(x: Ref^, y: Ref^)
//def par(op1: () => Unit, op2: () => Unit): Unit = ()
def foo(f: () => Ref^): Unit = ()
def main(): Unit =
  //val a = Ref(0)
  //val b = Ref(0)
  //par(() => a.data = 42, () => b.data = 42)

  val test = (x: Ref^) =>
    val t1 = x
    foo(() => x)

    //val t2 = Ref(0)
    //t2

