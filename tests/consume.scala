struct Ref(data: i32)
//struct Pair(x: Ref^, y: Ref^)
//def mkPair(consume a: Ref^, consume b: Ref^): Pair^ = Pair(a, b)
def main(): Unit =
  def foo(consume x: Ref^): Ref^ = x
  val a = Ref(0)
  #i32println(a.data)
  val f = () => () => foo(a)
  f()()
  #i32println(a.data)  // should be an error
  ()


