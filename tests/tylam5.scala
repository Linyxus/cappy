extension [T](x: T)
  def applied[U](f: T => U): U = f(x)
  def id: T = x
struct Ref(var data: i32)
def foo(x: Ref^): Unit =
  x.data = x.data + 100
def main(): Unit =
  val t1 = 0
  val inc = (x: i32) => x + 1
  val t2 = t1.applied[i32](inc)
  #i32println(t2)
  val t3 = t2.id
  #i32println(t3)
  val t4 = Ref(0)
  val t5 = t4.id
  t5.applied[Unit]: (x: Ref^{t5}) =>
    foo(x)
  #i32println(t4.data)

