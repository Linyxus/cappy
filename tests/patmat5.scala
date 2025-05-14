struct Ref()
enum Maybe[+T]:
  case Some(v: T)
  case None()
def main(): Unit =
  def test1[cap C](x: Maybe[Ref^{C}]): Ref^{C, cap} =
    val t1 =
      x match
        case Some(v) => #unbox(v)
        case None() => Ref()
    t1
