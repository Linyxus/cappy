struct Ref()
struct Res[cap C](data: Ref^{C}, kill: () ->{consume C} Unit)
def test[cap C](r: Res[{C}]^): Unit =
  val x = r.data
  x  // ok
  r.kill()
  x  // no

