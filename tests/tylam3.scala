struct Ref(var data: i32)
def id[T](x: T): T = x
def main(): Unit =
  val t1 = id[i32](0)
  val t2 = id[i32](42)
  #i32println(t1)
  #i32println(t2)
  val t3 = Ref(0)
  val t4 = #unbox(id[Ref^{t3}](t3))
  t4.data = 100
  #i32println(t3.data)
