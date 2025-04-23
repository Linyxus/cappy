struct Ref(var x: i32)
def main(): Unit =
  val a = Ref(0)
  val inc = () =>
    a.x = #i32add(a.x, 1)
  ()
