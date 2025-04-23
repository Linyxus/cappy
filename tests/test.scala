def main(): Unit =
  def twice(f: (x: i32) -> i32) = (x: i32) => f(f(x))
  def double(x: i32): i32 = #i32add(x, x)
  val f = twice(double)
  val x = #i32read()
  #i32println(f(x))
