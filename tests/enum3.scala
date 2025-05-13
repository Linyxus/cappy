struct Ref:
  var data: i32
struct Q(a: Ref^, b: Ref^, c: Ref^)
struct P(a: i32, b: i32, c: i32)
def showQ(q: Q^): Unit = 
  q match
    case q1 @ Q(a, b, c) =>
      putStrLn(">>>>>>>>")
      #i32println(a.data)
      #i32println(b.data)
      #i32println(c.data)
      putStrLn("<<<<<<<<")
def showP(p: P^): Unit =
  p match
    case P(x, y, z) =>
      #i32println(x + y * z)
def incQ(q: Q^): Unit =
  q match
    case Q(a, b, c) =>
      a.data = a.data + 1
      b.data = b.data * 2
      c.data = a.data - b.data
def main(): Unit =
  // showQ(Q(Ref(0), Ref(0)))
  // showP(P(100, 200, 300))
  val q = Q(Ref(1), Ref(1), Ref(1))
  showQ(q)
  (1.until(50)).iterate: (n: i32) =>
    incQ(q)
    showQ(q)
