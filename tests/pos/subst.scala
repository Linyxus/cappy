struct Ref()
def test(a: Ref^, b: Ref^): Unit =
  def f1(x: Ref^, y: Ref^{x}): Unit = ()
  f1(a, a)
  def f2(x: Ref^, y: Ref^{x}, z: Ref^{x}): Unit = ()
  f2(a, a, a)
  def f3(x: Ref^, y: Ref^): Unit = ()
  //f3(a, a) // error
  val f4 = Ref()
  ()
