def test(): Unit =
  val f = (a: i64, b: i64) => #i64add(a, b)
  val t = f(42, 42)
  ()
