struct Ref()
struct Pair(fst: Ref^, snd: Ref^)
def main(): Unit =
  val a = Ref()
  val b = Ref()
  val p = Pair(a, b)
  val t1 = p.fst
  val t2: Ref^{a} = t1
  val t3: Ref^{p} = t1
