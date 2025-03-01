val swap: [cap C1, C2] -> [T1, T2] -> (x: Ref[T1]^{C1}, y: Ref[T2]^{C2}) -> Unit = "???"
val x1: Ref[Int]^{cap} = newRef()
val x2: Ref[Int]^{cap} = newRef()
val test: () -> Unit = () =>
  swap[cap {x1}, {x1}][Int, Int](x1, x1)
  swap(x1, x2)

