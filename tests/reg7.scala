struct P(a: i32, b: i32)
def print(p: Ar[P]^): Unit =
  p match
    case P(a, b) => #i32println(a + b)
def main(): Unit = arena: zone =>
  val p1 = zone.P(10, 10)
  print(p1)
