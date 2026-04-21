import java.util.{ArrayList, Collections, HashMap, HashSet}
import java.util.concurrent.atomic.AtomicBoolean

import scala.python.runtime.PyThreading

private def joinAll(threads: Array[java.lang.Thread]): Unit =
  var i = 0
  while i < threads.length do
    threads(i).join()
    i += 1

@main def javalibCollectionsSynchronized(): Unit =
  val syncCollection = Collections.synchronizedCollection(new ArrayList[String]())
  syncCollection.add("red")
  syncCollection.add("blue")
  println("collection:" + syncCollection.size() + ":" + syncCollection.contains("blue"))

  val syncSet = Collections.synchronizedSet(new HashSet[String]())
  syncSet.add("alpha")
  syncSet.add("alpha")
  syncSet.add("beta")
  println("set:" + syncSet.size() + ":" + syncSet.contains("beta"))

  val syncMap = Collections.synchronizedMap(new HashMap[String, Int]())
  syncMap.put("one", 1)
  syncMap.put("two", 2)
  println(
    "map:" +
      syncMap.size() + ":" +
      syncMap.keySet().size() + ":" +
      syncMap.values().size() + ":" +
      syncMap.entrySet().size()
  )

  val syncList = Collections.synchronizedList(new ArrayList[Int]())
  val workerCount = 4
  val iterations = 250
  val start = PyThreading.newEvent()
  val threads = new Array[java.lang.Thread](workerCount)

  def worker(id: Int): Unit =
    start.waitReady()
    val base = id * iterations
    var i = 0
    while i < iterations do
      syncList.add(base + i)
      i += 1

  var i = 0
  while i < workerCount do
    val workerId = i
    threads(i) = new java.lang.Thread(() => worker(workerId), "sync-list-" + workerId)
    threads(i).start()
    i += 1

  start.set()
  joinAll(threads)

  var sum = 0
  i = 0
  while i < syncList.size() do
    sum += syncList.get(i)
    i += 1
  println("list-stress:" + syncList.size() + ":" + sum + ":" + syncList.contains(0) + ":" + syncList.contains(workerCount * iterations - 1))

  val attempted = PyThreading.newEvent()
  val mutated = new AtomicBoolean(false)
  val mutator = new java.lang.Thread(
    () => {
      attempted.set()
      syncList.add(workerCount * iterations)
      mutated.set(true)
    },
    "sync-list-mutator"
  )

  syncList.synchronized {
    mutator.start()
    attempted.waitReady()
    val sizeBefore = syncList.size()
    val iter = syncList.iterator()
    var iterated = 0
    while iter.hasNext() do
      iter.next()
      iterated += 1
    println("list-iter:" + sizeBefore + ":" + iterated + ":" + mutated.get())
  }

  mutator.join()
  println("list-after:" + syncList.size() + ":" + mutated.get())
