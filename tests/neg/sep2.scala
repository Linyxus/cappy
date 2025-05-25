struct Ref(var data: i32)
struct P(x: Ref^, y: Ref^)
def kill(consume x: Ref^): Unit = ()
def killP(consume p: P^): Unit = ()
def test1(consume p: P^): Unit =
  kill(p.x)
  p.y.data = 0  // error
def test2(consume x: Ref^, consume y: Ref^): Unit =
  x.data = 0
  y.data = 0
  kill(x)
  y.data = 0
  x.data = 0  // error
def test3(consume p: P^): Unit =
  p.x.data = 0
  killP(p)
  p.x.data = 42  // error

