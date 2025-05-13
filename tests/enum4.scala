struct P[A, B](a: A, b: B)
def main(): Unit =
  val p1 = P(0, 42)
  val p2 = P('0', true)
  val p3 = P(true, 42)
  val p4 = P(#box(p3), #box(p3))
