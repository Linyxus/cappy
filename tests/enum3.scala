struct Ref:
  var data: i32
struct Q(a: Ref^, b: Ref^)
// def mkQ: Q^ = Q(Ref(0), Ref(0))
def showQ(q: Q^): Unit = 
  q match
    case q1 @ Q(a, b) =>
      q1.a.data = 42
      q1.b.data = 100
      #i32println(a.data)
      #i32println(b.data)
def main(): Unit =
  // def mkQ1 = () => 
  //   val x = mkQ
  //   x.a
  showQ(Q(Ref(0), Ref(0)))
  ()
