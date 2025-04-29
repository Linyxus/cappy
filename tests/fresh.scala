struct Ref(var data: i32)
struct Pair(fst: Ref^, snd: Ref^)
def main(): Unit =
  val x = Ref(0)
  val y = Ref(0)
  val p = Pair(x, y)
  val t1: Ref^{x} = p.fst
  val t2: Ref^{y} = p.snd

  def foo(p: Pair^): Pair^{p} =
    p

