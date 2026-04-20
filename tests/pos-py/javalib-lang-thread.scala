final class SeededIntLocal extends java.lang.ThreadLocal[Int]:
  override protected def initialValue(): Int = 41

final class ProbeInheritable extends java.lang.InheritableThreadLocal[String]:
  def copy(parentValue: String): String = childValue(parentValue)

@main def javalibLangThread(): Unit =
  val current = java.lang.Thread.currentThread()
  println("currentthread:" + (current eq java.lang.Thread.currentThread()) + ":" + current.getName() + ":" + current.getId())

  current.interrupt()
  println("interrupted:" + current.isInterrupted() + ":" + java.lang.Thread.interrupted() + ":" + current.isInterrupted())

  val local = new java.lang.ThreadLocal[String]()
  local.set("ready")
  println("threadlocal-get-set:" + local.get())

  local.remove()
  println("threadlocal-remove:" + (local.get() == null))

  val seeded = new SeededIntLocal()
  println("threadlocal-initial-value:" + seeded.get())
  seeded.set(99)
  seeded.remove()
  println("threadlocal-initial-value-reset:" + seeded.get())

  val inheritable = new ProbeInheritable()
  println("inheritablethreadlocal:" + inheritable.copy("seed"))
