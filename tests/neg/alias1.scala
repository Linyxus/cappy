module hello
type T1 = i32
type T2[X] = X
type T3[X] = array[X]
def main(): Unit =
  val t1: T1 = 0
  val t2: T2[i32] = 0
  val t3: array[i32] = sorry()
  val t4: T3[i32] = t3
  val t5: array[i32] = t4
  val t6: T2[i32] = t5 // error
  ()

