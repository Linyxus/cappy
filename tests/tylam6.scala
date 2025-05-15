extension [T](x: T)
  def applied[U](op: T => U): U = op(x)
def add(x: i32): i32 = x + 1
def main(): Unit =
  val t1 = 41
  t1.applied[i32](add)
