struct Pair[A, B](var x: A, var y: B)

def mapArray(xs: array[i32]^, f: i32 => i32): Unit = ()

def main(): Unit =
  val xs = newArray[i32](10, 42)
  val p = Pair[i32, array[i32]^](0, xs) // limitation // inlining the definition of `xs` causes an error right now
  def f(x: i32): i32 =
    p.x = p.x + x
    x
  mapArray(p.y, f)  // error
  // requires {p} and {f} be separated, but {f} <: {p}

