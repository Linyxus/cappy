struct Ref()
def sep(x: Ref^, y: Ref^): Unit = ()
def test1(a: Ref^): Unit =
  sep(a, a)
def test2(a: Ref^, b: Ref^): Unit =
  sep(a, b)
  sep(b, b)
def main(): Unit =
  ()
