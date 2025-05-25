struct Ref(var data: i32)
struct P(x: Ref^, y: Ref^)
def useRef(x: Ref^): Unit = ()
def useP(p: P^): Unit = ()
def par(op1: () => Unit, op2: () => Unit): Unit = ()
def test1(p: P^): Unit =
  par(() => useP(p), () => useRef(p.x))  // error
def test2(p: P^): Unit =
  par(() => useRef(p.y), () => useRef(p.x))  // ok
  par(() => useRef(p.y), () => useRef(p.y))  // error
def test3(p: P^): Unit =
  val q = p
  par(() => useRef(q.y), () => useRef(p.x))  // ok
  par(() => useRef(q.y), () => useRef(p.y))  // error
