struct Ref(var data: i32)
def get(x: Ref^ro): i32 = x.data
def set(x: Ref^): Unit = x.data = 42
def main(): Unit =
  val a = Ref(0)
  val f1 = () => get(a)
  val f2 = (x: Ref^) => () => get(x)
  ()
