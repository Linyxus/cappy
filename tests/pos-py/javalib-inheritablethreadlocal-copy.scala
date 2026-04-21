import scala.python.runtime.PyThreading

final class CopyingLocal extends java.lang.InheritableThreadLocal[String]:
  override protected def childValue(parentValue: String): String =
    parentValue + "-child"

@main def javalibInheritablethreadlocalCopy(): Unit =
  val ready = PyThreading.newEvent()
  val release = PyThreading.newEvent()
  val local = new CopyingLocal()
  var childBefore = ""
  var childAfter = ""

  local.set("seed")

  val worker = new java.lang.Thread(() =>
    childBefore = local.get()
    ready.set()
    release.waitReady()
    childAfter = local.get()
  )

  worker.start()
  ready.waitReady()
  local.set("parent-mutated")
  release.set()
  worker.join()

  println("child:" + childBefore + ":" + childAfter)
  println("parent:" + local.get())
