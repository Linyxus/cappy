struct Ref(data: i32)
struct Pair(a: Ref^, b: Ref^)
def sep(a: Ref^, b: Ref^): Unit = ()
def sep1(a: Ref^, p: Pair^): Unit = ()
def test(): Unit =
  def f(p: Pair^): Unit =
    val t1 = p
    val t2 = p.a
    val t3 = p.b
    sep(t2, t3)
    //sep1(t2, p)  // error
    sep1(t3, p)
  ()
