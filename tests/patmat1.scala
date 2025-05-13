enum V:
  case Up()
  case Down()
enum H:
  case Left()
  case Right()
struct Move(v: V^, h: H^)
def show(m: Move^): Unit =
  m match
    case Move(Up(), Left()) => #i32println(0)
    case Move(Up(), Right()) => #i32println(1)
    case Move(Down(), _) => #i32println(2)
def main(): Unit =
  show(Move(Up(), Right()))
  show(Move(Down(), Right()))
  show(Move(Down(), Left()))
  show(Move(Up(), Left()))
  ()
