struct Ref(var value: i32)
extension (x: Ref^)
  def update(op: i32 => i32): Unit =
    x.value = op(x.value)
def main(): Unit =
  val a = Ref(0)
  #i32println(a.value)
  a.update: (x: i32) =>
    x + 100
  #i32println(a.value)
  a.update: (x: i32) =>
    x * x * 200
  #i32println(a.value)
  ()
