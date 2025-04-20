def test(): Unit =
  val f1 = [X] => (x: X) => x
  val t2 = f1[i64](0)
  val f3 = [X, Y <: X] => (x: Y) => x
  val t4 = f3[i64, i64](0)
  val f5 = [X, Y <: X, Z <: X] => 0
  val t6 = f5[i32, i32, i32]
  val t7 = f5[i32, i32, i64]
