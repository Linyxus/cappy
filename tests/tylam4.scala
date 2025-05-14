struct Ref(var data: i32)
def main(): Unit = 
  def id[T](x: T): T = x
  val t1 = id[i32](0)
  #i32println(t1)
  val t2 = Ref(0)
  val t3 = #unbox(id[Ref^{t2}](#box(t2)))
  t3.data = 100
  #i32println(t2.data)
