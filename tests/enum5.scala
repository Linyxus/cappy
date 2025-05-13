enum Expr:
  case Int(v: i32)
  case Bool(b: bool)
  case Char(ch: char)
def show(e: Expr^): Unit =
  e match
    case Int(_) => #i32println(0)
    case Bool(_) => #i32println(1)
    case Char(_) => #i32println(2)
def main(): Unit =
  val t1 = Int(0)
  val t2 = Bool(false)
  val t3 = Char('\0')
  show(t1)
  show(t2)
  show(t3)
