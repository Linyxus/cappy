struct Ref(var data: i32)
struct Pair(x: Ref^, y: Ref^)
//def par(op1: () => Unit, op2: () => Unit): Unit = ()
def main(): Unit =
  val a = Ref(0)
  val b = Ref(0)
  val p = Pair(a, b)
  val t1 = p.x
  val t2 = () => p.x
  val t3: Ref^{a} = t1
  val t4: Ref^{p} = t1
  val t5: Ref^{b} = p.y
  val t6: Ref^{p} = p.y

