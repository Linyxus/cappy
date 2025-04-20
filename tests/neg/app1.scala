def test(): Unit =
  val f = (a: i64, b: i64) => #i64add(a, b)
  val t1 = f(42, 42)
  val t2 = f(42, f)
  ()
