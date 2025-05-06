struct Pair[+A, +B](fst: A, snd: B)
struct Ref(var data: i32)
def mk: Pair[() => i32, () => i32]^ =
  val x = Ref(0)
  val inc = () =>
    x.data = x.data + 1
    x.data
  val dec = () =>
    x.data = x.data - 1
    x.data
  val p = Pair[() ->{inc} i32, () ->{dec} i32](inc, dec)
  p
def main(): Unit = ()
