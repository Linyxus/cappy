import java.util.function.Supplier

final class SeedSupplier extends Supplier[Int]:
  override def get(): Int = 7

@main def javalibThreadlocalPerThread(): Unit =
  val local = java.lang.ThreadLocal.withInitial(new SeedSupplier())
  local.set(10)

  var childBefore = -1
  var childAfter = -1

  val worker = new java.lang.Thread(() =>
    childBefore = local.get()
    local.set(99)
    childAfter = local.get()
  )

  worker.start()
  worker.join()

  println("main:" + local.get())
  println("child:" + childBefore + ":" + childAfter)
