def main(): i32 =
  def add(a: i32)(b: i32) = #i32add(a, b)
  def twice(f: (z: i32) -> i32)(x: i32): i32 = f(f(x))
  val addOne = add(1)
  val addTwo = twice(addOne)
  addTwo(40)

