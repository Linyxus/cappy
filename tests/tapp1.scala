def test(): Unit =
  val f1 = [X] => (x: X) => x
  val t2 = f1[i64](0)
