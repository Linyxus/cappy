struct Ref(var data: i32)
struct RefPair(x: Ref^, y: Ref^)
def par(op1: () => Unit, op2: () => Unit): Unit = ()
def main(): Unit =
  val a = Ref(0)
  val b = Ref(0)
  par(() => a.data = 42, () => b.data = 42)
  ()
