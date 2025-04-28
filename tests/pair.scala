struct Ref(var data: i32)
struct Pair(x: Ref^, y: Ref^)
struct Pair1[T, U](x: T, y: U)
def sep(x: Ref^, y: Ref^): Unit = ()
def test[cap C1, cap C2](p: Pair1[Ref^{C1}, Ref^{C2}]): Unit =
  sep(p.x, p.y)

  
