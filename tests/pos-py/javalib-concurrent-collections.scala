import java.util.concurrent.*
import scala.python.runtime.PyThreading

private def joinAll(threads: Array[java.lang.Thread]): Unit =
  var i = 0
  while i < threads.length do
    threads(i).join()
    i += 1

@main def javalibConcurrentCollections(): Unit =
  val map = new ConcurrentHashMap[String, Int]()
  val mapStart = PyThreading.newEvent()
  val mapThreads = Array(
    new java.lang.Thread(() =>
      mapStart.waitReady()
      var i = 0
      while i < 100 do
        map.put("a-" + i, i)
        i += 1
    ),
    new java.lang.Thread(() =>
      mapStart.waitReady()
      var i = 0
      while i < 100 do
        map.put("b-" + i, 100 + i)
        i += 1
    ),
    new java.lang.Thread(() =>
      mapStart.waitReady()
      var i = 0
      while i < 100 do
        map.put("c-" + i, 200 + i)
        i += 1
    ),
    new java.lang.Thread(() =>
      mapStart.waitReady()
      var i = 0
      while i < 100 do
        map.put("d-" + i, 300 + i)
        i += 1
    )
  )
  var mapIndex = 0
  while mapIndex < mapThreads.length do
    mapThreads(mapIndex).start()
    mapIndex += 1
  mapStart.set()
  joinAll(mapThreads)
  val mapIter = map.entrySet().iterator()
  map.put("late", 999)
  var mapSnapshotCount = 0
  while mapIter.hasNext() do
    mapIter.next()
    mapSnapshotCount += 1
  val keySet = ConcurrentHashMap.newKeySet[String]()
  keySet.add("x")
  keySet.add("y")
  println("concurrenthashmap:" + map.size() + ":" + mapSnapshotCount + ":" + map.containsKey("late") + ":" + keySet.size())

  val queue = new ConcurrentLinkedQueue[String]()
  val queueStart = PyThreading.newEvent()
  val queueThreads = Array(
    new java.lang.Thread(() =>
      queueStart.waitReady()
      var i = 0
      while i < 100 do
        queue.offer("q1-" + i)
        i += 1
    ),
    new java.lang.Thread(() =>
      queueStart.waitReady()
      var i = 0
      while i < 100 do
        queue.offer("q2-" + i)
        i += 1
    ),
    new java.lang.Thread(() =>
      queueStart.waitReady()
      var i = 0
      while i < 100 do
        queue.offer("q3-" + i)
        i += 1
    ),
    new java.lang.Thread(() =>
      queueStart.waitReady()
      var i = 0
      while i < 100 do
        queue.offer("q4-" + i)
        i += 1
    )
  )
  var queueIndex = 0
  while queueIndex < queueThreads.length do
    queueThreads(queueIndex).start()
    queueIndex += 1
  queueStart.set()
  joinAll(queueThreads)
  val queueIter = queue.iterator()
  queue.offer("late")
  var queueSnapshotCount = 0
  while queueIter.hasNext() do
    queueIter.next()
    queueSnapshotCount += 1
  println("concurrentlinkedqueue:" + queue.size() + ":" + queueSnapshotCount + ":" + queue.contains("late"))

  val set = new ConcurrentSkipListSet[Int]()
  val setStart = PyThreading.newEvent()
  val setThreads = Array(
    new java.lang.Thread(() =>
      setStart.waitReady()
      var i = 0
      while i < 100 do
        set.add(i)
        i += 1
    ),
    new java.lang.Thread(() =>
      setStart.waitReady()
      var i = 100
      while i < 200 do
        set.add(i)
        i += 1
    ),
    new java.lang.Thread(() =>
      setStart.waitReady()
      var i = 200
      while i < 300 do
        set.add(i)
        i += 1
    ),
    new java.lang.Thread(() =>
      setStart.waitReady()
      var i = 300
      while i < 400 do
        set.add(i)
        i += 1
    )
  )
  var setIndex = 0
  while setIndex < setThreads.length do
    setThreads(setIndex).start()
    setIndex += 1
  setStart.set()
  joinAll(setThreads)
  val setIter = set.iterator()
  set.add(999)
  var setSnapshotCount = 0
  while setIter.hasNext() do
    setIter.next()
    setSnapshotCount += 1
  println("concurrentskiplistset:" + set.size() + ":" + setSnapshotCount + ":" + set.first() + ":" + set.last())

  val list = new CopyOnWriteArrayList[String]()
  val listStart = PyThreading.newEvent()
  val listThreads = Array(
    new java.lang.Thread(() =>
      listStart.waitReady()
      var i = 0
      while i < 100 do
        list.add("l1-" + i)
        i += 1
    ),
    new java.lang.Thread(() =>
      listStart.waitReady()
      var i = 0
      while i < 100 do
        list.add("l2-" + i)
        i += 1
    ),
    new java.lang.Thread(() =>
      listStart.waitReady()
      var i = 0
      while i < 100 do
        list.add("l3-" + i)
        i += 1
    ),
    new java.lang.Thread(() =>
      listStart.waitReady()
      var i = 0
      while i < 100 do
        list.add("l4-" + i)
        i += 1
    )
  )
  var listIndex = 0
  while listIndex < listThreads.length do
    listThreads(listIndex).start()
    listIndex += 1
  listStart.set()
  joinAll(listThreads)
  val listIter = list.iterator()
  list.add("late")
  val lateDuplicateAdded = list.addIfAbsent("late")
  var listSnapshotCount = 0
  while listIter.hasNext() do
    listIter.next()
    listSnapshotCount += 1
  println("copyonwritearraylist:" + list.size() + ":" + listSnapshotCount + ":" + lateDuplicateAdded + ":" + list.get(list.size() - 1))
