struct Ref:
  var data: i32
struct Q(a: Ref^, b: Ref^)
struct P(a: i32, b: i32, c: i32)
def showQ(q: Q^): Unit = 
  q match
    case q1 @ Q(a, b) =>
      q1.a.data = 42
      q1.b.data = 100
      #i32println(a.data)
      #i32println(b.data)
def showP(p: P^): Unit =
  p match
    case P(x, y, z) =>
      #i32println(x + y * z)
def main(): Unit =
  showQ(Q(Ref(0), Ref(0)))
  showP(P(100, 200, 300))
