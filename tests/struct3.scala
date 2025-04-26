struct Ref(var data: i32)
struct Pair(x: Ref^, y: Ref^)
//def par(op1: () => Unit, op2: () => Unit): Unit = ()
def main(): Unit =
  val a = Ref(0)
  val b = Ref(0)
  val p = Pair(a, b)
  val t1 = p.x

