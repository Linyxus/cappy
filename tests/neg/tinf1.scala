struct A(var x: i32)
def id[T](x: T): T = x
def test1(): Unit =
  val t1 = id(0)
  val t2 = id(false)
  val t3 = id: () =>
    ()
  val t4 = id(A(0))  // error

