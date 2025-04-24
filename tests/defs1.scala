def add(a: i32, b: i32): i32 = #i32add(a, b)
val one: i32 = 1
val two: i32 = add(one, one)
val three: i32 = add(two, one)
val four: i32 = add(two, two)
def square(x: i32): i32 = #i32mul(x, x)
def main(): Unit =
  val x = add(one, one)
  #i32println(x)
  #i32println(square(four))
