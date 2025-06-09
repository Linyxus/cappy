enum ArListBase:
  case ANil()
  case ACons(head: i32, tail: Ar[ArListBase]^)
type ArList = Ar[ArListBase]
def enumerate(until: i32, zone: Arena^): ArList^{zone} =
  def recur(cur: i32, acc: ArList^{zone}): ArList^{zone} =
    if cur < 0 then acc
    else recur(cur - 1, zone.ACons(cur, acc))
  recur(until - 1, zone.ANil())
def print(xs: ArList^): Unit =
  xs match
    case ANil() => ()
    case ACons(h, t) =>
      #i32println(h)
      print(t)
def sum(xs: ArList^): i32 =
  def recur(xs: ArList^, acc: i32): i32 = xs match
    case ANil() => acc
    case ACons(h, t) => recur(t, h + acc)
  recur(xs, 0)
def run(): Unit = arena: z =>
  val xs1 = enumerate(50000, z)
  #i32println(sum(xs1))
def main(): Unit = 
  run()
  run()
  run()
