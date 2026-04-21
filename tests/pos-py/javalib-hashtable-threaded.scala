import java.util.Hashtable
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.python.runtime.PyThreading

private def joinAll(threads: Array[java.lang.Thread]): Unit =
  var i = 0
  while i < threads.length do
    threads(i).join()
    i += 1

@main def javalibHashtableThreaded(): Unit =
  val writerCount = 4
  val iterations = 500
  val totalKeys = writerCount * iterations
  val table = new Hashtable[String, String]()
  val start = PyThreading.newEvent()
  val broken = new AtomicBoolean(false)
  val writersDone = new AtomicInteger(0)
  val threads = new Array[java.lang.Thread](writerCount + 1)
  val keys = new Array[String](totalKeys)
  val values = new Array[String](totalKeys)

  var fillIndex = 0
  while fillIndex < totalKeys do
    keys(fillIndex) = "k" + fillIndex
    values(fillIndex) = "v" + fillIndex
    fillIndex += 1

  def keyOf(index: Int): String =
    keys(index)

  def expectedValue(index: Int): String =
    values(index)

  def writer(id: Int): Unit =
    start.waitReady()
    val base = id * iterations
    var i = 0
    while i < iterations do
      val index = base + i
      table.put(keyOf(index), expectedValue(index))
      i += 1
    writersDone.incrementAndGet()

  def reader(): Unit =
    start.waitReady()
    while writersDone.get() < writerCount && !broken.get() do
      var index = 0
      while index < totalKeys && !broken.get() do
        val key = keyOf(index)
        table.synchronized {
          if table.containsKey(key) && (table.get(key).asInstanceOf[AnyRef] ne expectedValue(index).asInstanceOf[AnyRef]) then
            broken.set(true)
        }
        index += 1

  var i = 0
  while i < writerCount do
    val writerId = i
    threads(i) = new java.lang.Thread(() => writer(writerId), "ht-writer-" + writerId)
    threads(i).start()
    i += 1

  threads(writerCount) = new java.lang.Thread(() => reader(), "ht-reader")
  threads(writerCount).start()

  start.set()
  joinAll(threads)

  var missing = 0
  var index = 0
  while index < totalKeys do
    if table.get(keyOf(index)).asInstanceOf[AnyRef] ne expectedValue(index).asInstanceOf[AnyRef] then
      missing += 1
    index += 1

  println("threaded:" + table.size() + ":" + missing + ":" + broken.get())
