enum Result[+T]:
  case Ok(v: T)
  case Error()
def foo(x: Result[i32]^): i32 = 42
def bar(x: Ok[i32]^): i32 = x.v
def baz(x: Error[i32]^): i32 = -1
def test1(x: Ok[i32]^): Result[i32]^{x} = x
def test2(x: Error[i32]^): Result[i32]^{x} = x
def main(): Unit =
  ()
