struct Ref(data: i32)
def main(): Unit =
  def foo(consume x: Ref^): Ref^ = x
  val a = Ref(0)
  #i32println(a.data)
  val f = () => () => foo(a)
  f()()
  #i32println(a.data)  // error


