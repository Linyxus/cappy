struct Ref(var data: i32)
def get(x: Ref^ro): Unit = x.data
def set(x: Ref^): Unit = x.data = 42
def par(op1: () => Unit, op2: () => Unit): Unit = ()
def kill(consume x: Ref^): Unit = ()
def main(): Unit =
  val a = Ref(0)
  val b = Ref(0)
  par(() => get(a), () => get(a))  // ok
  //par(() => get(a), () => set(a))  // no
  par(() => get(a), () => get(b))  // of course ok
  par(() => set(a), () => get(b))  // of course ok
  par(() => get(a), () => set(b))  // of course ok
  par(() => set(a), () => set(b))  // of course ok
  kill(a)
  //get(a) // no
  get(b) // ok
  set(b) // ok
  //set(a)  // no
