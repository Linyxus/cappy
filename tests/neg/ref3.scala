struct Ref(var x: i32)
def swap(x: Ref^, y: Ref^): Unit = ()
def swap1(x: () -> () => Ref^, y: () -> () => Ref^): Unit = ()
def main(): Unit =
  val a = Ref(0)
  val b = Ref(0)
  swap(a, b)
  swap1(() => () => a, () => () => b)
  swap1(() => () => a, () => () => a)  // should be a consume error, but now ok
  swap(a, a) // error
