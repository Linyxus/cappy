struct Ref()
struct Pair(fst: Ref^, snd: Ref^)
def kill(consume x: Ref^): Unit = ()
def main(): Unit =
  val a = Ref()
  val b = Ref()
  val p = Pair(a, b)
  kill(a)
  b  // ok
  p.snd
  ()
