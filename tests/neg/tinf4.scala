struct Cov[+T]()
struct Cot[-T]()
def test1(): Unit =
  def foo[T](a: Cov[T]^, b: Cot[T]^): Unit = ()
  foo(Cov[i32](), Cot[i32]())
  foo(Cov[i32](), Cot[bool]())  // error
