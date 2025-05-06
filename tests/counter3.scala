struct Pair[+A, +B](fst: A, snd: B)
struct Ref(var data: i32)
type Counter[+cap C] = Pair[() ->{C} i32, () ->{C} i32]
def mk: Counter[{cap}]^ =
  val x = Ref(0)
  val inc = () =>
    x.data = x.data + 1
    x.data
  val dec = () =>
    x.data = x.data - 1
    x.data
  val p = Pair[() ->{inc} i32, () ->{dec} i32](inc, dec)
  val p1: Counter[{x}]^{p} = p
  p1
def main(): Unit = ()
