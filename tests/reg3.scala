struct P:
  var x: i32
  var y: i32
struct MP:
  var a: Ar[P]^
  var b: Ar[P]^
def main(): Unit = arena: zone =>
  def sum(p: Ar[P]^{zone}): i32 = p.x + p.y
  val p1 = zone.P(1, 2)
  val p2 = zone.P(3, 4)
  val m1 = zone.MP(p1, p2)
  0

