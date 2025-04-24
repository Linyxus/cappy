struct P(var x: i32, var y: i32)
def main(): Unit =
  val p = P(42, 0)
  #i32println(p.x)
  p.y = 100
  #i32println(p.y)
