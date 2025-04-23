def main(): i32 =
  def compose(f: (z: i32) -> i32, g: (z: i32) -> i32)(x: i32) = g(f(x))
  def f1(x: i32) = #i32mul(x, 2)
  def f2(x: i32) = #i32add(x, 1)
  val h = compose(f1, f2)
  h(20)

