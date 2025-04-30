struct Ref()
struct Pair(a: Ref^, b: Ref^)
def kill(consume x: Ref^): Unit = ()
def main(): Unit =
  val p = Pair(Ref(), Ref())
  kill(p.a)
  p  // error
  ()
