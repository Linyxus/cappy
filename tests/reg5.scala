struct Ref(var data: i32)
struct Runner(var op: () => Unit)
def main(): Unit = arena[Unit]: zone =>
  val a = zone.Ref(0)
  val f = () =>
    val now = a.data
    a.data = a.data + 1
    #i32println(now)
  val runner = zone.Runner(f)
  f()
  f()
  runner.op()
  runner.op()
  runner.op()
  runner.op()
  runner.op()
  runner.op = () => #i32println(42)
  runner.op()
  runner.op()
  runner.op()

