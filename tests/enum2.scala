enum F:
  case A(x: i32)
  case B(x: bool)
def mkF(): F^ = B(false)
def test1(f: F^): i32 = f match
  case A(x) => x
  case B(x) => 42
def test2(f: F^): i32 = 
  f match
    case A(x) => x
    case B(x) => 42
  0
def test3(): i32 = mkF() match
  case A(x) => x
  case B(x) => 0
def main(): Unit =
  val t1 = A(0)
  t1 match
    case A(x) => x
