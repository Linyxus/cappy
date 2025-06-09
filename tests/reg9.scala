enum ArListBase:
  case ANil()
  case ACons(head: i32, tail: Ar[ArListBase]^)
type ArList = Ar[ArListBase]
def enumerate(until: i32, zone: Arena^): ArList^{zone} =
  def recur(cur: i32): ArList^{zone} =
    if cur >= until then zone.ANil()
    else zone.ACons(cur, recur(cur+1))
  recur(0)
def print(xs: ArList^): Unit =
  xs match
    case ANil() => ()
    case ACons(h, t) =>
      #i32println(h)
      print(t)
def main(): Unit = arena: z =>
  val xs1 = enumerate(100, z)
  print(xs1)
