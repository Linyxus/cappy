struct Ref(var data: i32)
def test(a: Ref^, b: Ref^): Unit =
  def foo[T, U](a: T, b: U): Unit = ()
  val x = foo[i32, Ref^{a}]
  //foo[i32, Ref^{a}](0, a)

