enum Maybe:
  case Ok(value: i32)
  case None()
def show(m: Ar[Maybe]^): Unit =
  m match
    case Ok(_) => putStrLn("is ok")
    case None() => putStrLn("is none")
def main(): Unit = arena: zone =>
  val x1 = zone.Ok(0)
  val x2 = zone.None()
  show(x1)
  show(x2)
