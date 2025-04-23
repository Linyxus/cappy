struct Ref(var value: i32)
def main(): Unit =
  val r1 = Ref(0)
  #i32println(r1.value)
  r1.value = #i32add(r1.value, 1)
  #i32println(r1.value)
  ()
