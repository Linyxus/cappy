struct A(var x: i32)
struct B(a: A^)
def main(): Unit = arena[Unit]: zone =>
  val a = A(42)
  val a1 = zone.A(42)
  val b = zone.B(a)
  val a2 = b.a
  #i32println(a2.x)
  a2.x = 100
  #i32println(a.x)
