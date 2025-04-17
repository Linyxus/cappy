val swap: [cap C1, C2] -> (x: Ref^{C1}, y: Ref^{C2}) -> Unit = 0
val x1: Ref^{cap} = newRef()
val x2: Ref^{cap} = newRef()
val test: () -> Unit = () =>
  swap[{x1}, {x1}](x1, x1)
  swap(x1, x2)

