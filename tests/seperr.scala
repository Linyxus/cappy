struct Ref()
def sep(a: Ref^, b: Ref^): Unit = ()
def main(): Unit =
  val x = Ref()
  //sep(x, x)
  def foo(y: Ref^): Unit = sep(x, y)
  foo(x)
