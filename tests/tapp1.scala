def test(): Unit =
  val f1 = [X] => (x: X) => x
  val t2 = f1[i64](0)
  val f3 = [X, Y <: X] => (x: Y) => x
  val t4 = f3[i64, Int](0)
